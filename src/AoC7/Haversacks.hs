module AoC7.Haversacks where

import Common.Utils
import Data.List.Split
import qualified Data.Map as M

aoc7 :: IO ()
aoc7 = do
  contents <- getInputFile 7
  print $ parseInput contents
  let bagRules = parseInput contents
  print $ M.lookup (BG "dotted" "black") bagRules

type BagRules = M.Map Bag (M.Map Bag Int)

data Bag = BG
  { hue :: String,
    colour :: String
  }
  deriving (Show, Eq, Ord)

parseInput :: String -> BagRules
parseInput = M.fromList . map parseLine . lines

parseLine :: String -> (Bag, M.Map Bag Int)
parseLine str = (container, bags)
  where
    [containerStr, bagsStr] = splitOn " contain " str
    container = parseContainer containerStr
    bags = parseBags bagsStr

parseBags :: String -> M.Map Bag Int
parseBags str = case str of
  "no other bags." -> M.empty
  _ -> M.fromList $ map toBagTuple bagList
  where
    bagList = splitOn "," str
    toBagTuple s =
      let [number, hue, colour, _] = words s
       in (BG hue colour, read number)

parseContainer :: String -> Bag
parseContainer str = BG hue colour
  where
    [hue, colour, _] = words str

