{-# LANGUAGE TupleSections #-}

module AoC20.JurassicJigsaw where

import Common.Utils
import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Product (Product))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Tuple
import Debug.Trace
import Linear.V2
import Linear.Vector
import System.Directory
import System.IO
import Text.Format
import Control.Lens

aoc20 :: IO ()
aoc20 = do
  contents <- getInputFile 20
  let tileMap = parseContents contents
  --putStr $ fromJust $ part2 tileMap
  monster <- readMonster
  let chart = fromJust $ tester tileMap monster
  putStr $ renderVectorMap chart
  print "done"

data Tile = MkTile
  { _id :: Int,
    _grid :: Grid
  }
  deriving (Eq, Show, Ord) -- Use ID for ord?

data Cardinal = North | East | South | West deriving (Eq, Show, Enum, Ord, Bounded)

type TileMap = M.Map Int Tile

type Grid = M.Map (V2 Int) Char

type Borders = M.Map Cardinal String

data MatchCriteria = MC
  { _sourceCardinal :: Cardinal,
    _destinationCardinal :: Cardinal,
    _destinationCoord :: V2 Int,
    _isFlipped :: Bool
  }
  deriving (Show, Eq, Ord)

data TileMatch = TM
  { _tileId :: Int,
    _matchingSide :: Cardinal,
    _flipped :: Bool
  }
  deriving (Show, Eq, Ord)

data Flip = Horizontal | Vertical | None deriving (Eq, Show, Ord, Enum)

data PlacementParams = PP
  { placedTileMap :: PlacedTileMap,
    queue :: [Tile]
  }
  deriving (Eq, Show, Ord)

type PlacedTileMap = M.Map (V2 Int) Tile

type Chart = M.Map (V2 Int) Char

part2 :: TileMap -> Maybe String
part2 tileMap = do
  (PP result queue) <- run $ initPlacementParams tileMap
  let withoutBorders = M.map stripBorder result
  pure $ printPlacedTileMap withoutBorders


tester :: TileMap -> Chart -> Maybe Chart
tester tileMap monster = do
  (PP result queue) <- run $ initPlacementParams tileMap
  let withoutBorders = translateToOrigin $ M.map stripBorder result
  let chart = translateToOrigin $ foldTileMap withoutBorders
  rotateFlipAndScan monster chart


rotateFlipAndScan :: Chart -> Chart -> Maybe Chart
rotateFlipAndScan monster chart = do
  let unflippedCombos = map (`rotateChart` chart) [0..3]
  --let flippedCombos = map (`rotateChart` chart) [0..3]
  let scans = filter containsSeaMonster $ map (scanForSeaMonsters monster) unflippedCombos
  if null scans then Nothing else Just $ head scans

containsSeaMonster :: Chart -> Bool
containsSeaMonster chart = '0' `elem` M.elems chart

scanForSeaMonsters :: Chart -> Chart -> Chart
scanForSeaMonsters monster chart = folded
    where folded = M.foldlWithKey scanRegion M.empty chart
          scanRegion :: Chart -> V2 Int -> Char -> Chart
          scanRegion aggregator coord value = let region = regionForCoord chart coord
                                   in if isSeaMonster monster region
                                      then let revealed = revealSeaMonster monster region in M.union revealed aggregator
                                      else M.union aggregator region

regionForCoord :: Chart -> V2 Int -> Chart
regionForCoord chart (V2 x y) = M.filterWithKey (\(V2 thisX thisY) _ -> 
                                                    thisX >= xMin
                                                    && thisX < xMax
                                                    && thisY >= yMin
                                                    && thisY < yMax
                                                    ) chart
  where xMin = x
        xMax = 20+x
        yMin = y
        yMax = 3+y

isSeaMonster :: Chart -> Chart -> Bool
isSeaMonster monster region = M.intersection justHashes monster == monster
  where
    movedToOrigin = translateToOrigin region
    justHashes = M.filter (== '#') movedToOrigin

revealSeaMonster :: Chart -> Chart -> Chart
revealSeaMonster monster region = M.unionWith (\_ _ -> '0') translatedMonster region
  where keys = M.keysSet region
        xs = S.map (^. _x) keys
        ys = S.map (^. _y) keys
        xmin = minimum xs
        ymin = minimum ys
        translatedMonster = M.mapKeys (\v -> v + V2 xmin ymin) monster

initPlacementParams :: TileMap -> PlacementParams
initPlacementParams tm = PP M.empty (M.elems tm)

translateToOrigin :: M.Map (V2 Int) a -> M.Map (V2 Int) a
translateToOrigin placedTileMap = M.mapKeys (+ movementVector) placedTileMap
  where
    keys = M.keysSet placedTileMap
    (V2 minx _) = minimumBy (\(V2 x1 y1) (V2 x2 y2) -> compare x1 x2) keys
    (V2 _ miny) = minimumBy (\(V2 x1 y1) (V2 x2 y2) -> compare y1 y2) keys
    movementVector = V2 (- minx) (- miny)

stripBorder :: Tile -> Tile
stripBorder (MkTile id grid) = MkTile id translated
  where
    strippedGrid = M.filterWithKey (\(V2 x y) t -> not (x `elem` [0, 9] || y `elem` [0, 9])) grid
    translated = M.mapKeys (\v -> v + V2 (-1) (-1)) strippedGrid

--Border as a string to tuple of the tile's cardinal and it's location
getFreeBorders :: PlacedTileMap -> M.Map String (Cardinal, V2 Int)
getFreeBorders placedTileMap = borderMap
  where
    freeNeighbours = M.filter (not . (`M.member` placedTileMap)) . orthogonalNeighbours
    tilesToFreeNeighbours = M.filter (not . null) $ M.mapWithKey (\k v -> freeNeighbours k) placedTileMap
    borderMap = M.foldlWithKey lookUpTileAndGetBorders M.empty tilesToFreeNeighbours
    lookUpTileAndGetBorders mp coord cardinalMap =
      let tile = placedTileMap M.! coord
          borders = M.fromList $ map (\c -> (borderForCardinal tile c, (c, coord))) $ M.keys cardinalMap
       in M.union mp borders

run :: PlacementParams -> Maybe PlacementParams
run pp@(PP placedTileMap queue)
  | null queue = Just pp
  | otherwise = step pp >>= run

step :: PlacementParams -> Maybe PlacementParams
step pp@(PP placedTileMap queue)
  | null placedTileMap = Just $ placeAnywhere pp
  | null queue = Just pp
  | otherwise =
    let placed = placeFromQueue pp
     in if isNothing placed
          then Just $ PP placedTileMap (bumpQueue queue)
          else placed
  where
    bumpQueue (first : rest) = rest ++ [first]

placeFromQueue :: PlacementParams -> Maybe PlacementParams
placeFromQueue pp@(PP placedTileMap (firstTile : rest)) = do
  let freeBorders = getFreeBorders placedTileMap
  let tileBorders = borders (_grid firstTile)
  (MC sourceCardinal destinationCardinal destinationCoord flipped) <- findFirstMatch freeBorders tileBorders
  let placementCoord = orthogonalNeighbours destinationCoord M.! destinationCardinal
  let rotation = matchCardinalRotation sourceCardinal destinationCardinal
  let newTileNotFlipped = rotateTile rotation firstTile
  let newTile =
        if flipped --Don't bother flipping for now to check i can do rotation properly
          then flipForDestinationCardinal destinationCardinal newTileNotFlipped
          else newTileNotFlipped
  pure $ PP (M.insert placementCoord newTile placedTileMap) rest

--Clockwise rotation you will need to match destination cardinal to source cardinal
matchCardinalRotation :: Cardinal -> Cardinal -> Int
matchCardinalRotation source destination = reverseCardinalList !! lookupIndex
  where
    reverseCardinalList = map fromEnum [South, East, North, West]
    lookupIndex = (fromEnum source - fromEnum destination) `mod` 4

findFirstMatch :: M.Map String (Cardinal, V2 Int) -> Borders -> Maybe MatchCriteria
findFirstMatch freeBorders tileBorders = do
  let tileBorderValues = values tileBorders
  let firstFlippedMatch = S.lookupMin $ S.intersection (M.keysSet freeBorders) tileBorderValues
  let firstNonFlippedMatch = S.lookupMin $ S.intersection (M.keysSet freeBorders) (S.map reverse tileBorderValues)
  let nonFlipped = (,) <$> firstNonFlippedMatch <*> Just False
  let flipped = (,) <$> firstFlippedMatch <*> Just True
  (match, flipped) <- nonFlipped <|> flipped
  let (destinationCardinal, destinationCoord) = freeBorders M.! match
  sourceCardinal <- getCardinal tileBorders (if flipped then match else reverse match)
  pure $ MC sourceCardinal destinationCardinal destinationCoord flipped

placeAnywhere :: PlacementParams -> PlacementParams
placeAnywhere (PP placedTileMap (firstTile : rest)) =
  let newMap = M.insert (V2 0 0) firstTile placedTileMap
   in PP newMap rest

values :: (Ord a) => M.Map k a -> S.Set a
values = S.fromList . M.elems

getCardinal :: Borders -> String -> Maybe Cardinal
getCardinal borders border = M.lookup border reversed
  where
    reversed = M.fromList $ map swap $ M.toList borders

orthogonalNeighbours :: V2 Int -> M.Map Cardinal (V2 Int)
orthogonalNeighbours coord = M.fromList [(North, north), (East, east), (South, south), (West, west)]
  where
    north = coord + V2 0 (-1)
    east = coord + V2 1 0
    south = coord + V2 0 1
    west = coord + V2 (-1) 0

--This is how the border would read if you rotated it to the top of the tile
borders :: Grid -> Borders
borders grid = M.fromList [(North, north), (East, east), (South, reverse south), (West, reverse west)]
  where
    north = foldUp $ M.filterWithKey (\(V2 _ y) _ -> y == 0) grid
    east = foldUp $ M.filterWithKey (\(V2 x _) _ -> x == 9) grid
    south = foldUp $ M.filterWithKey (\(V2 _ y) _ -> y == 9) grid
    west = foldUp $ M.filterWithKey (\(V2 x y) _ -> x == 0) grid
    foldUp = M.foldMapWithKey (\k v -> [v])

borderForCardinal :: Tile -> Cardinal -> String
borderForCardinal (MkTile _ grid) cardinal = borders grid M.! cardinal

parseContents :: String -> TileMap
parseContents str = M.fromList tileList
  where
    tilesStr = splitOn "\n\n" str
    tileList = map (extractToTuple . parseTileStr) tilesStr
    extractToTuple tile = (_id tile, tile)

parseTileStr :: String -> Tile
parseTileStr str = MkTile (read id) grid
  where
    idStr : gridStr = lines str
    grid = enumerateMultilineStringToVectorMap $ intercalate "\n" gridStr
    id = filter (/= ':') $ words idStr !! 1

flipTile :: Flip -> Tile -> Tile
flipTile flip (MkTile id grid) = MkTile id (flipGrid flip grid)

flipForDestinationCardinal :: Cardinal -> Tile -> Tile
flipForDestinationCardinal cardinal tile
  | cardinal `elem` [North, South] = flipTile Horizontal tile
  | cardinal `elem` [West, East] = flipTile Vertical tile

flipGrid :: Flip -> Grid -> Grid
flipGrid flip grid = M.mapWithKey flipper grid
  where
    flipper coord _ = grid M.! flipCoord flip coord

maxXAndY :: Chart -> V2 Int
maxXAndY chart = V2 xmax ymax
  where keys = M.keysSet chart
        xs = S.map (^. _x) keys
        ys = S.map (^. _y) keys
        xmax = maximum xs
        ymax = maximum ys

flipCoord :: Flip -> V2 Int -> V2 Int
flipCoord flip (V2 x y) =
  let flipNum num = fromJust $ Seq.fromList [9, 8 .. 0] Seq.!? num
   in case flip of
        Horizontal -> V2 (flipNum x) y
        Vertical -> V2 x (flipNum y)

rotateTile :: Int -> Tile -> Tile
rotateTile amount (MkTile id grid) = MkTile id rotatedGrid
  where
    rotatedGrid = M.mapKeys (rotateCoord amount) grid

rotateChart :: Int -> M.Map (V2 Int) a -> M.Map (V2 Int) a
rotateChart amount = translateToOrigin . M.mapKeys (rotateCoord amount)

--0 is no rotation, 1 is 90 degrees clockwise etc
rotateCoord :: Int -> V2 Int -> V2 Int
rotateCoord amount coord = iterate rotatedAboutOrigin coord !! amount
  where
    rotatedAboutOrigin (V2 x y) = V2 9 0 + V2 (- y) x

printTile :: Tile -> IO ()
printTile (MkTile _ grid) = putStr $ renderVectorMap grid

printPlacedTileMap :: PlacedTileMap -> String
printPlacedTileMap placedTileMap = renderVectorMap $ foldTileMap placedTileMap

foldTileMap :: PlacedTileMap -> M.Map (V2 Int) Char
foldTileMap placedTileMap = if null placedTileMap then M.empty else M.foldMapWithKey foldTile placedTileMap

foldTile :: V2 Int -> Tile -> M.Map (V2 Int) Char
foldTile macroCoord (MkTile _ grid) = M.foldMapWithKey (foldGrid macroCoord) grid
  where
    foldGrid (V2 macroX macroY) (V2 x y) ch =
      let newX = (gridSize * macroX + x)
          newY = (gridSize * macroY + y)
       in M.singleton (V2 newX newY) ch
    gridSize = (+) 1 $ maximum $ map (\(V2 x y) -> x) $ M.keys grid

runInteractive :: PlacementParams -> IO ()
runInteractive pp@(PP placedTileMap queue) = do
  putStr $ printPlacedTileMap placedTileMap
  _ <- getLine
  let stepped = step pp
  runInteractive (fromJust stepped)
  print "done"

readMonster :: IO Chart
readMonster = do
  workingDirectory <- getCurrentDirectory
  let path = format "{0}/res/AoC20/monster.txt" [workingDirectory]
  contents <- readFileToString path
  let monster = M.filter (=='#') $ enumerateMultilineStringToVectorMap contents
  return monster