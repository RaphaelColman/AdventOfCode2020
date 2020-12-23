{-# LANGUAGE TupleSections #-}
module AoC20.JurassicJigsaw where

import           Common.Utils
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Monoid     (Product (Product))
import qualified Data.Sequence   as Seq
import qualified Data.Set        as S
import           Linear.V2
import           Linear.Vector

aoc20 :: IO ()
aoc20 = do
    contents <- getInputFile  20
    let tileMap = parseContents contents
    let aTile = tileMap M.! 2311
    let aBorder = borders (_grid aTile) M.! North
    let rotated = rotateTile 1 aTile
    --print $ part1 tileMap
    print "done"

data Tile = MkTile {
    _id   :: Int,
    _grid :: Grid
} deriving (Eq, Show, Ord) -- Use ID for ord?

data Cardinal = North | East | South | West deriving (Eq, Show, Enum, Ord, Bounded)

type TileSet = S.Set Tile
type TileMap = M.Map Int Tile
type Grid = M.Map (V2 Int) Char
type Borders = M.Map Cardinal String
type SiblingMap = M.Map Cardinal (M.Map Int [MatchCriteria])
type MatchCriteria = (Cardinal, Bool) --The cardinal on the target and whether you have flipped it
type DeterministicTileMap = M.Map Int (M.Map Cardinal TileMatch)

data TileMatch = TM {
    _tileId       :: Int,
    _matchingSide :: Cardinal,
    _flipped      :: Bool
} deriving (Show, Eq, Ord)

data Flip = Horizontal | Vertical | None deriving (Eq, Show, Ord, Enum)

--Naive for part 1. Assuming the borders are mostly unique, if there are four tiles with only 2 matches then we know they must be the corner tiles
part1 :: M.Map Int Tile -> Product Int
part1 tileMap = M.foldMapWithKey (\k v -> Product k) only2
    where lengths = M.map (length . findSiblings tileMap) tileMap
          only2 = M.filter (==2) lengths


data RelativeTilePlacement = RTP {
    _relativePlacementTileId :: Int,
    _cardinal :: Cardinal,
    _rotation :: Int,
    _flip :: Flip
} deriving (Eq, Show, Ord)

data AbsoluteTilePlacement = ATP {
    _absolutePlacementTileId :: Int,
    _location :: V2 Int,
    _absoluteRotation :: Int,
    _absoluteFlip :: Flip
} deriving (Eq, Show, Ord)

convertToAbsolute :: AbsoluteTilePlacement -> RelativeTilePlacement -> AbsoluteTilePlacement
convertToAbsolute (ATP _ location rotation flip) newPlacement = undefined
    where newCardinal oldCardinal = let notFlippedYet = stepEnum oldCardinal rotation
                                    in case flip of
                                            Horizontal -> case notFlippedYet of
                                                                West -> East
                                                                East -> West
                                                                other -> other
                                            Vertical -> case notFlippedYet of
                                                                North -> South
                                                                South -> North
                                                                other -> other
                                            None -> notFlippedYet
          newRotation oldRotation = oldRotation + rotation

--List of tileplacements relative to a source tile presuming the source tile has not been flipped or rotated.
findRelativeTilePlacement :: DeterministicTileMap -> Tile -> [RelativeTilePlacement]
findRelativeTilePlacement dtm (MkTile id _) = mapMaybe forCardinal [North, East, South, West]
    where forCardinal cardinal = do
             (TM id matchingSide flipped) <- M.lookup id dtm >>= M.lookup cardinal
             rotation <- (+ fromEnum cardinal) <$> elemIndex matchingSide [South, East, North, West]
             let flip = if flipped
                        then if matchingSide `elem` [North, South] then Horizontal else Vertical
                        else None
             pure $ RTP id cardinal rotation flip


findSiblings :: TileMap -> Tile -> SiblingMap
findSiblings tileMap tile@(MkTile _id _grid) = M.filter (not . null) siblingMap
    where borders' = borders _grid
          siblingMap = M.map (findSiblingForBorder tileMap _id) borders'

--for a border return map of (Matching tile id) -> Cardinals on it which match
findSiblingForBorder :: TileMap -> Int -> String -> M.Map Int [MatchCriteria]
findSiblingForBorder tileMap originalTileId border = M.unionWith (++) nonFlippedMatches flippedMatches
    where tileMatches border' tile@(MkTile id' grid') isFlipped = let otherBorders = borders grid'
                                                in map (, isFlipped) $ M.keys $ M.filter (\b -> b == if isFlipped then border' else reverse border') otherBorders
          findMatches isFlipped = M.filter (not . null) $ M.mapWithKey (\_id tile -> tileMatches border tile isFlipped) allExceptOriginal
          nonFlippedMatches = findMatches False
          flippedMatches = findMatches True
          allExceptOriginal = M.filterWithKey (\k _ -> k /= originalTileId) tileMap

--This is how the border would read if you rotated it to the top of the tile
borders :: Grid -> Borders
borders grid = M.fromList [(North, north), (East, east), (South, reverse south), (West, reverse west)]
    where north = foldUp $ M.filterWithKey (\(V2 _ y) _ -> y==0) grid
          east = foldUp $ M.filterWithKey (\(V2 x _) _ -> x==9) grid
          south = foldUp $ M.filterWithKey (\(V2 _ y) _ -> y==9) grid
          west = foldUp $ M.filterWithKey (\(V2 x y) _ -> x==0) grid
          foldUp = M.foldMapWithKey (\k v -> [v])


parseContents :: String -> TileMap
parseContents str = M.fromList tileList
    where tilesStr = splitOn "\n\n" str
          tileList = map (extractToTuple . parseTileStr) tilesStr
          extractToTuple tile = (_id tile, tile)

parseTileStr :: String -> Tile
parseTileStr str = MkTile (read id) grid
    where idStr:gridStr = lines str
          grid = enumerateMultilineStringToVectorMap $ intercalate "\n" gridStr
          id = filter (/= ':') $ words idStr !! 1


attemptSimplify :: TileMap -> DeterministicTileMap
attemptSimplify tileMap = simplified
    where allMatches = M.map (findSiblings tileMap) tileMap
          simplified = M.map simplify allMatches

simplify :: SiblingMap -> M.Map Cardinal TileMatch
simplify = M.map simplifyMap
    where simplifyMap :: M.Map Int [MatchCriteria] -> TileMatch
          simplifyMap mp
            | length mp == 1 = let [(onlyId, criteria)] = M.toList mp
                                in if length criteria /= 1
                                    then error "Multiple criteria for match"
                                    else let (onlyCardinal, onlyBool) = head criteria in TM onlyId onlyCardinal onlyBool
            | otherwise = error "Multiple matches for cardinal"

flipTile :: Flip -> Tile -> Tile
flipTile flip (MkTile id grid) = MkTile id (flipGrid flip grid)

flipGrid :: Flip -> Grid -> Grid
flipGrid flip grid = M.mapWithKey flipper grid
    where flipper coord _ = grid M.! flipCoord flip coord

flipCoord :: Flip -> V2 Int -> V2 Int
flipCoord flip (V2 x y) =
    let flipNum num = fromJust $ Seq.fromList [9,8..0] Seq.!? num
    in case flip of
    Horizontal -> V2 (flipNum x) y
    Vertical   -> V2 x (flipNum y)


rotateTile :: Int -> Tile -> Tile
rotateTile amount (MkTile id grid) = MkTile id rotatedGrid
    where rotatedGrid = M.mapKeys (rotateCoord amount) grid

--0 is no rotation, 1 is 90 degrees clockwise etc
rotateCoord :: Int -> V2 Int -> V2 Int
rotateCoord amount coord = iterate rotatedAboutOrigin coord !! amount
    where rotatedAboutOrigin (V2 x y) = V2 9 0 + V2 (-y) x

printTile :: Tile -> IO ()
printTile (MkTile _ grid) = putStr $ renderVectorMap grid
