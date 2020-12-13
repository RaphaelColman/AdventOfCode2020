{-# LANGUAGE ScopedTypeVariables #-}
module AoC13.ShuttleSearch where

import Common.Utils
import Data.List
import Data.List.Split
import Data.Maybe
import Prelude hiding (id)
import Data.Sort
import qualified GHC.Exts as Data.Ord

aoc13 :: IO ()
aoc13 = do
    contents <- getInputFile 13
    let (target, times) = parseContents contents
    print $ part1 target times
    let busTimes = parseForPart2 contents
    print $ part2 busTimes

parseContents :: String -> (Int, [Int])
parseContents str = (read targetStr, times)
    where [targetStr, timesStr] = lines str
          times :: [Int] = map read $ filter (/= "x") $ splitOn "," timesStr

parseForPart2 :: String -> [BusInfo]
parseForPart2 str = asMap
    where inputList = splitOn "," times
          [target, times] = lines str
          asMap = map (\(a,b) -> BI (read a) b)
                $ filter ((/="x") . fst)
                $ zip inputList [0..]

part1 :: Int -> [Int] -> Int
part1 target times = (time - target) * busId
    where (time, busId) = earliestBusTime target times

part2 :: [BusInfo] -> Int
part2 = findValidTime . sortOn Data.Ord.Down 

earliestBusTime :: (Ord b, Num b, Enum b) => b -> [b] -> (b, b)
earliestBusTime target = minimumBy (\(earliest, _) (otherEarliest, _) -> compare earliest otherEarliest) . map mapTimes
    where mapTimes x = let earliest = fromJust $ find (>=target ) [x,2*x..]
                        in (earliest, x)

data BusInfo = BI {
    id :: Int,
    offset :: Int
} deriving (Show, Eq, Ord)

findValidTime :: [BusInfo] -> Int
findValidTime ((BI id offset):rest) = go id 1 offset id rest
    where go time phase offset' target [] 
            | (time + offset') `mod` target == 0 = time
            | otherwise = go (time+phase) phase offset' target []
          go time phase offset' target bis'@((BI id' offset''):rest')
            | (time + offset') `mod` target == 0 =  go time (phase*target) offset'' id' rest'
            | otherwise = go (time+phase) phase offset' target bis'

