module AoC10.AdapterArray where

import           Common.Utils
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Sort

import qualified Data.IntMap     as IM
import qualified Data.IntSet     as IS
import Debug.Trace

aoc10 :: IO ()
aoc10 = do
  contents <- getInputFile 10
  let adapters = parseContents contents
  let sortedAdapters = sort adapters
  print $ freqs $ joltDistribution sortedAdapters
  print $ combinations sortedAdapters
  print $ findPathsFrom0 $ toAdapterIntSet sortedAdapters

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

pathsToGoal :: IS.IntSet -> IM.IntMap Int
pathsToGoal is = res
  where res = IM.fromSet lookupOrCalculate is
        lookupOrCalculate x = if x == goal
                then 1
                else sum [ IM.findWithDefault 0 (x + increment) res | increment <- [1,2,3]]
        goal = IS.findMax is

toAdapterIntSet :: [Int] -> IS.IntSet
toAdapterIntSet xs = IS.union (IS.fromList [0, top + 3]) originals
                where top = IS.findMax originals
                      originals = IS.fromList xs

findPathsFrom0 :: IS.IntSet -> Int
findPathsFrom0 = IM.findWithDefault 0 0 . pathsToGoal

fooPathToGoal :: IS.IntSet -> IM.IntMap [[Int]]
fooPathToGoal is = res
  where res = IM.fromSet lookUpOrCalculate is
        lookUpOrCalculate x = if x == goal
          then [[x]]
          else let nextThreePaths = [ IM.findWithDefault [] (x+increment) res | increment <- [1,2,3]] 
                in map (x :) $ concat nextThreePaths
        goal = IS.findMax is

fooFindPathsFrom0 :: IS.IntSet -> Maybe [[Int]]
fooFindPathsFrom0 = IM.lookup 0 . fooPathToGoal