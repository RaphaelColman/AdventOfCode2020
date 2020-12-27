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
import Linear.V2
import Linear.Vector

aoc20 :: IO ()
aoc20 = do
  contents <- getInputFile 20
  let tileMap = parseContents contents
  let aTile = tileMap M.! 2311
  let aBorder = borders (_grid aTile) M.! North
  --print $ part1 tileMap
  print "done"

data Tile = MkTile
  { _id :: Int,
    _grid :: Grid
  }
  deriving (Eq, Show, Ord) -- Use ID for ord?

data Cardinal = North | East | South | West deriving (Eq, Show, Enum, Ord, Bounded)

type TileSet = S.Set Tile

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

step :: PlacementParams -> Maybe PlacementParams
step pp@(PP placedTileMap queue)
  | null placedTileMap = Just $ placeAnywhere pp
  | null queue = Just pp
  | otherwise = placeFromQueue pp

placeFromQueue :: PlacementParams -> Maybe PlacementParams
placeFromQueue pp@(PP placedTileMap (firstTile : rest)) = do
  let freeBorders = getFreeBorders placedTileMap
  let tileBorders = borders (_grid firstTile)
  (MC sourceCardinal destinationCardinal destinationCoord flipped) <- findFirstMatch freeBorders tileBorders
  let placementCoord = orthogonalNeighbours destinationCoord M.! destinationCardinal
  --TODO work out rotation, perform rotation, put in map
  Nothing

findFirstMatch :: M.Map String (Cardinal, V2 Int) -> Borders -> Maybe MatchCriteria
findFirstMatch freeBorders tileBorders = do
  let tileBorderValues = values tileBorders
  let firstNonFlippedMatch = S.lookupMin $ S.intersection (M.keysSet freeBorders) tileBorderValues
  let firstFlippedMatch = S.lookupMin $ S.intersection (M.keysSet freeBorders) (S.map reverse tileBorderValues)
  let nonFlipped = (,) <$> firstNonFlippedMatch <*> Just True
  let flipped = (,) <$> firstFlippedMatch <*> Just False
  (match, flipped) <- nonFlipped <|> flipped
  let (destinationCardinal, destinationCoord) = freeBorders M.! match
  sourceCardinal <- getCardinal tileBorders match
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

flipGrid :: Flip -> Grid -> Grid
flipGrid flip grid = M.mapWithKey flipper grid
  where
    flipper coord _ = grid M.! flipCoord flip coord

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

--0 is no rotation, 1 is 90 degrees clockwise etc
rotateCoord :: Int -> V2 Int -> V2 Int
rotateCoord amount coord = iterate rotatedAboutOrigin coord !! amount
  where
    rotatedAboutOrigin (V2 x y) = V2 9 0 + V2 (- y) x

printTile :: Tile -> IO ()
printTile (MkTile _ grid) = putStr $ renderVectorMap grid