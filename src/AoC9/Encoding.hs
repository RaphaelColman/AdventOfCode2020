module AoC9.Encoding where

import Common.Utils
import Data.List

aoc9 :: IO()
aoc9 = do
    contents <- getInputFile 9
    let ints = parseInput contents
    let firstInvalidNumber =  fst . head $ dropUntilInvalid 25 ints
    print firstInvalidNumber
    print $ convert $ findContiguousSetToSum firstInvalidNumber ints

parseInput :: String -> [Int]
parseInput = map read . lines

summableFromList :: Int -> [Int] -> Bool
summableFromList i = any (\(a,b) -> a + b == i) . pairs

dropUntilInvalid :: Int -> [Int] -> [(Int, [Int])]
dropUntilInvalid preambleLength xs = dropWhile (uncurry summableFromList) tups
    where listFromFirstRealElement = drop preambleLength xs
          tls = map (take preambleLength) $ filter (\ls -> length ls >= preambleLength) $ tails xs
          tups = zip listFromFirstRealElement tls

findContiguousSetToSum :: Int -> [Int] -> [Int]
findContiguousSetToSum target xs = head $ filter (\l -> sum l == target) allPossibleLists
    where allPossibleLists = concatMap inits (tails xs)

convert :: [Int] -> Int
convert xs = minimum xs + maximum xs