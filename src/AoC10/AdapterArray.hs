module AoC10.AdapterArray where

import Common.Utils
import Data.Sort
import qualified Data.Map as M
import Data.List
import Data.List.Split

aoc10 :: IO ()
aoc10 = do
  contents <- getInputFile 10
  let adapters = parseContents contents
  let sortedAdapters = sort adapters
  print $ freqs $ joltDistribution sortedAdapters

parseContents :: String -> [Int]
parseContents = map read . lines

differenceDistribution :: [Int] -> [Int]
differenceDistribution xs = zipWith (-) (tail xs) xs

--add the first and last differences to the beginning of the list
joltDistribution :: [Int] -> [Int]
joltDistribution xs = 3 : head xs : differenceDistribution xs


sumToWith3s2s1s:: Int -> Int
sumToWith3s2s1s i = undefined

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l
  | n > 0 = take n l : groupN n (drop n l)
  | otherwise = error "Negative or zero n"