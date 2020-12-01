module AoC1.Receipts where

import Data.List
import System.IO

main :: IO ()
main = do
  handle <- openFile "src/AoC1/input.txt" ReadMode  
  contents <- hGetContents handle
  let expenses = parseContents contents
  --let result = uncurry (*) $ goodPair $ pairs expenses
  let result = multiplyTriplet $ goodTriplet $ triplets expenses
  print result

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