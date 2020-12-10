module AoC10.AdapterArray where

import           Common.Utils
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Sort

aoc10 :: IO ()
aoc10 = do
  contents <- getInputFile 10
  let adapters = parseContents contents
  let sortedAdapters = sort adapters
  print $ freqs $ joltDistribution sortedAdapters
  print $ combinations sortedAdapters

parseContents :: String -> [Int]
parseContents = map read . lines

differenceDistribution :: [Int] -> [Int]
differenceDistribution xs = zipWith (-) (tail xs) xs

--add the first and last differences to the beginning of the list
joltDistribution :: [Int] -> [Int]
joltDistribution xs = (head xs : differenceDistribution xs) ++ [3]


sumToWith3s2s1s :: Int -> [[Int]]
sumToWith3s2s1s = go
  where go :: Int-> [[Int]]
        go target
            | target < 0 = []
            | target == 0 = [[]]
            | otherwise = map (1 :) (go (target - 1))
            ++ map (2 :) (go (target - 2))
            ++ map (3 :) (go (target - 3))

combinations :: [Int] -> Int
combinations xs = product $ map (length . sumToWith3s2s1s) groupsOfOneLengths
  where distribution = joltDistribution xs
        groupsOfOneLengths = map length $ filter (all (== 1)) $ group distribution
