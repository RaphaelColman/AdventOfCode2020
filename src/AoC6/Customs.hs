module AoC6.Customs where

import Common.Utils
import Data.List.Split
import qualified Data.Set as S

aoc6 :: IO ()
aoc6 = do
  contents <- getInputFile 6
  print $ splitToGroups contents
  print $ sumUnique $ splitToGroups contents
  print $ sumIntersections $ splitToGroups contents

splitToGroups :: String -> [[String]]
splitToGroups = map lines . splitOn "\n\n"

sumUnique :: [[String]] -> Int
sumUnique = sum . map (length . S.fromList. concat)

sumIntersections :: [[String]] -> Int
sumIntersections ll = sum $ map forSingleGroup ll

intersectionFoldableSets :: (Ord a, Eq a) => [S.Set a] -> S.Set a
intersectionFoldableSets sets = foldl S.intersection (head sets) sets

testGroups :: [[String]]
testGroups = [["abc"],["a","b","c"],["ab","ac"],["a","a","a","a"],["b"]]

forSingleGroup :: Ord a => [[a]] -> Int
forSingleGroup xs = length $ intersectionFoldableSets $ map S.fromList xs