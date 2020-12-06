module AoC6.Customs where

import Common.Utils
import Data.List.Split
import qualified Data.Set as S

aoc6 :: IO ()
aoc6 = do
  contents <- getInputFile 6
  print $ sumUnique $ splitToGroups contents

splitToGroups :: String -> [[String]]
splitToGroups = map lines . splitOn "\n\n"

sumUnique :: [[String]] -> Int
sumUnique = sum . map (length . S.fromList. concat )