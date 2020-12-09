module AoC7.Haversacks where

import           Common.Utils
import           Data.List.Split
import qualified Data.Map        as M
import qualified Data.Set        as S

aoc7 :: IO ()
aoc7 = do
  contents <- getInputFile 7
  let bagRules = parseInput contents
  let bagToFind = BG "shiny" "gold"
  print $ length $ recursiveContainersForBag bagToFind bagRules
  print $ numberOfBagsForBag bagToFind bagRules
  let ancestors = M.lookup bagToFind $ allAncestors bagRules
  print $ length <$> ancestors
  print $ M.lookup bagToFind $ usageCounts bagRules

type BagRules = M.Map Bag (M.Map Bag Int)

data Bag = BG
  {
    hue    :: String,
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
  _                -> M.fromList $ map toBagTuple bagList
  where
    bagList = splitOn "," str
    toBagTuple s =
      let [number, hue, colour, _] = words s
       in (BG hue colour, read number)

parseContainer :: String -> Bag
parseContainer str = BG hue colour
  where
    [hue, colour, _] = words str

recursiveContainersForBag :: Bag -> BagRules -> S.Set Bag
recursiveContainersForBag = go S.empty
  where go found bg bgRules
            | null containers = found
            | otherwise = let allContainers = S.map (\x -> go containers x bgRules) containers in S.foldl S.union found allContainers
            where containers = containersForBag bg bgRules
                  containersForBag b = M.keysSet . M.filter (M.member b)


numberOfBagsForBag :: Bag -> BagRules -> Int
numberOfBagsForBag bag bagRules
  | null childBags = 0
  | otherwise = numberOfChildBags + grandChildren
  where childBags = M.findWithDefault M.empty bag bagRules
        numberOfChildBags = M.foldl (+) 0 childBags
        grandChildren = M.foldlWithKey (\count bag' number' -> count + number' * numberOfBagsForBag bag' bagRules) 0 childBags


type Graph v e = M.Map v (M.Map v e)

allDescendants :: Ord v => Graph v e -> M.Map v (S.Set v)
allDescendants graph = descendantMap
  where
    descendantMap = M.foldMapWithKey (\v _ -> S.insert v (M.findWithDefault S.empty v descendantMap)) <$> graph

flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [ (m, M.singleton n e) | (n, ms) <- M.toList mp , (m, e) <- M.toList ms ]


allAncestors :: Ord v => Graph v e -> M.Map v (S.Set v)
allAncestors = allDescendants . flipGraph


usageCounts :: (Ord v, Num e) => Graph v e -> M.Map v e
usageCounts gr = usageMap
  where usageMap = (\mp -> sum [n * (M.findWithDefault 0 v usageMap + 1) | (v, n) <- M.toList mp]) <$> gr