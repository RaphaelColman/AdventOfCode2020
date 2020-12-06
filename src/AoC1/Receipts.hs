module AoC1.Receipts where

import           Common.Utils
import           Data.List

aoc1 :: IO ()
aoc1 = do
  contents <- getInputFile 1
  let expenses = parseContents contents
  print $ uncurry (*) $ goodPair $ pairs expenses
  print $ multiplyTriplet $ goodTriplet $ triplets expenses

parseContents :: String -> [Int]
parseContents = map read . lines

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

triplets :: [a] -> [(a, a, a)]
triplets l = [(x,y,z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

goodPair :: [(Int, Int)] -> (Int, Int)
goodPair = head . filter (\p -> uncurry (+) p == 2020)

goodTriplet :: [(Int, Int, Int)] -> (Int, Int, Int)
goodTriplet = head . filter (\(a, b, c) -> a + b + c == 2020)

multiplyTriplet :: (Num a) => (a, a, a) -> a
multiplyTriplet (x, y ,z) = x * y * z
