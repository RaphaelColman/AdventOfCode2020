{-# LANGUAGE ScopedTypeVariables #-}
module AoC13.ShuttleSearch where

import Common.Utils
import Data.List
import Data.List.Split
import Data.Maybe

aoc13 :: IO ()
aoc13 = do
    contents <- getInputFile 13
    let (target, times) = parseContents contents
    print $ part1 target times

parseContents :: String -> (Int, [Int])
parseContents str = (read targetStr, times)
    where [targetStr, timesStr] = lines str
          times :: [Int] = map read $ filter (/= "x") $ splitOn "," timesStr


part1 :: Int -> [Int] -> Int
part1 target times = (time - target) * busId
    where (time, busId) = earliestBusTime target times

earliestBusTime :: (Ord b, Num b, Enum b) => b -> [b] -> (b, b)
earliestBusTime target = minimumBy (\(earliest, _) (otherEarliest, _) -> compare earliest otherEarliest) . map mapTimes
    where mapTimes x = let earliest = fromJust $ find (>=target ) [x,2*x..]
                        in (earliest, x)
    