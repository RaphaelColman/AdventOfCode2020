module AoC24.LobbyLayout where

import Common.Utils
import Text.Trifecta
import Control.Applicative

aoc24 :: IO ()
aoc24 = do
    contents <- getInputFile 24
    let directionLists = parseString parseInput mempty contents
    print directionLists
    print "done"

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Enum, Eq, Show, Ord, Bounded)
type DirectionList = [Direction]

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
        _ -> fail "Invalid composite direction"