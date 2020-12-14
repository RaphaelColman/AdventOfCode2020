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


sumCount321 :: Int -> Int
sumCount321 x = IM.findWithDefault 0 x res
  where res = IM.fromSet mapToCount $ IS.fromList [1..x]
        mapToCount key
          | key < 0 = 0
          | key == 0 = 0
          | key == 1 = 1
          | otherwise = let nextKeys = filter (>=0) [key - x | x <- [1, 2, 3]]
                            lookedUp = map (\x -> IM.findWithDefault 0 x res) nextKeys
                            zeros = length $ filter (== 0) nextKeys
                        in sum lookedUp + zeros


combinations :: [Int] -> Int
combinations xs = product $ map sumCount321 groupsOfOneLengths
  where distribution = joltDistribution xs
        groupsOfOneLengths = map length $ filter (all (== 1)) $ group distribution



--Another solution using recursively tying the knot and memoisation

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