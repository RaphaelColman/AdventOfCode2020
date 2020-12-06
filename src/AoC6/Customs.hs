module AoC6.Customs where

import           Common.Utils
import           Data.List.Split
import qualified Data.Set        as S

aoc6 :: IO ()
aoc6 = do
  contents <- getInputFile 6
  let groups = splitToGroups contents
  print $ sumUnique groups
  print $ sumIntersections groups

splitToGroups :: String -> [[String]]
splitToGroups = map lines . splitOn "\n\n"

sumUnique :: [[String]] -> Int
sumUnique = sum . map (length . S.fromList. concat)

sumIntersections :: [[String]] -> Int
sumIntersections = sum . map countIntersections
    where countIntersections = length
                                . foldl1 S.intersection
                                . map S.fromList