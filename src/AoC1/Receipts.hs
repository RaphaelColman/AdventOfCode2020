module AoC1.Receipts where

import Data.List
import System.IO

main :: IO ()
main = do
  handle <- openFile "src/AoC1/input.txt" ReadMode  
  contents <- hGetContents handle
  let expenses = parseContents contents
  let result = uncurry (*) $ goodPair $ pairs expenses
  print result

parseContents :: String -> [Int]
parseContents = map read . lines

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

goodPair :: [(Int, Int)] -> (Int, Int)
goodPair = head . filter (\p -> uncurry (+) p == 2020)
