module AoC24.LobbyLayout where

import           Common.Utils
import           Control.Applicative
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Linear.V2
import           Text.Trifecta

aoc24 :: IO ()
aoc24 = do
    contents <- getInputFile 24
    let directionLists = parseString parseInput mempty contents
    print $ part1 <$> directionLists
    print "done"

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Enum, Eq, Show, Ord, Bounded)
type DirectionList = [Direction]
type Coord = V2 Int

part1 :: [DirectionList] -> Int
part1 dls = length $ M.filter odd freqMap
    where freqMap = toTileFlips dls

toTileFlips :: [DirectionList] -> Map Coord Int
toTileFlips = freqs . map getToTile

getToTile :: DirectionList -> V2 Int
getToTile = foldl' (\a v -> a + directionToVector v) (V2 0 0)

--Doing this with the origin in the bottom left
directionToVector :: Direction -> V2 Int
directionToVector direction = case direction of
                                East      -> V2 2 0
                                SouthEast -> V2 1 (-1)
                                SouthWest -> V2 (-1) (-1)
                                West      -> V2 (-2) 0
                                NorthWest -> V2 (-1) 1
                                NorthEast -> V2 1 1

--Parsing
parseInput :: Parser [DirectionList]
parseInput = some (token parseLine)

parseLine :: Parser DirectionList
parseLine = some parseDirection

parseDirection :: Parser Direction
parseDirection = try parseEastWest <|> try parseComposite

parseEastWest :: Parser Direction
parseEastWest = do
    eastWest <- oneOf "ew"
    case eastWest of
        'e' -> return East
        'w' -> return West


parseComposite :: Parser Direction
parseComposite = do
    directionStr <- count 2 anyChar
    case directionStr of
        "se" -> return SouthEast
        "sw" -> return SouthWest
        "ne" -> return NorthEast
        "nw" -> return NorthWest
        _    -> fail "Invalid composite direction"

--Examples
itself = "nwwswee"
southEast = "esew"
