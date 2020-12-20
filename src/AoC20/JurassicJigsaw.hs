{-# LANGUAGE TupleSections #-}
module AoC20.JurassicJigsaw where

import           Common.Utils
import           Data.List.Split
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Linear.V2
import           Linear.Vector
import Data.List
import Data.Monoid (Product(Product))

aoc20 :: IO ()
aoc20 = do
    contents <- getInputFile  20
    let tileMap = parseContents contents
    let aTile = tileMap M.! 2311
    let aBorder = borders (_grid aTile) M.! North
    print $ part1 tileMap
    print "done"

data Tile = MkTile {
    _id   :: Int,
    _grid :: Grid
} deriving (Eq, Show, Ord) -- Use ID for ord?

data Cardinal = North | East | South | West deriving (Eq, Show, Enum, Ord)

type TileSet = S.Set Tile
type TileMap = M.Map Int Tile
type Grid = M.Map (V2 Int) Char
type Borders = M.Map Cardinal String
type SiblingMap = M.Map Cardinal (M.Map Int [MatchCriteria])
type MatchCriteria = (Cardinal, Bool) --The cardinal on the target and whether you have flipped it

--Naive for part 1. Assuming the borders are mostly unique, if there are four tiles with only 2 matches then we know they must be the corner tiles
part1 :: M.Map Int Tile -> Product Int
part1 tileMap = M.foldMapWithKey (\k v -> Product k) only2
    where lengths = M.map (length . findSiblings tileMap) tileMap
          only2 = M.filter (==2) lengths

findSiblings :: TileMap -> Tile -> SiblingMap
findSiblings tileMap tile@(MkTile _id _grid) = M.filter (not . null) siblingMap
    where borders' = borders _grid
          siblingMap = M.map (findSiblingForBorder tileMap _id) borders'

--for a border return map of (Matching tile id) -> Cardinals on it which match
findSiblingForBorder :: TileMap -> Int -> String -> M.Map Int [MatchCriteria]
findSiblingForBorder tileMap originalTileId border = M.unionWith (++) nonFlippedMatches flippedMatches
    where tileMatches border' tile@(MkTile id' grid') isFlipped = let otherBorders = borders grid'
                                                in map (, isFlipped) $ M.keys $ M.filter (\b -> b == if isFlipped then border' else reverse border') otherBorders
          nonFlippedMatches = M.filter (not . null) $ M.mapWithKey (\_id tile -> tileMatches border tile False) allExceptOriginal
          flippedMatches = M.filter (not . null) $ M.mapWithKey (\_id tile -> tileMatches border tile True) allExceptOriginal
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
