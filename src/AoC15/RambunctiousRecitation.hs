module AoC15.RambunctiousRecitation where

import qualified Data.IntMap.Strict as IM
import Data.Foldable (Foldable(foldl'))
import Debug.Trace

aoc15 :: IO ()
aoc15 = do
  print $ recite input 2020
  print $ recite input 30000000

input :: [Int]
input = [18, 8, 0, 5, 4, 1, 20]

recite :: [Int] -> Int -> Int
recite ints goal = currentNumber $ foldl' reciteFold params ll
  where
    params = toParams ints 
    ll = [len .. (goal-1)]
    len = length ints

data Params = PS
  { indexMap :: IM.IntMap Int,
    currentNumber :: Int
  }
  deriving (Eq, Show)

reciteFold :: Params -> Int -> Params
reciteFold (PS im currentNumber') count' = newParams
  where
    lastIndex = IM.findWithDefault 0 currentNumber' im
    nextNum = if lastIndex == 0 then 0 else count' - lastIndex
    newMap = IM.insert currentNumber' count' im
    newParams = PS newMap nextNum

toParams :: [Int] -> Params
toParams [] = undefined
toParams xs = PS (IM.fromList restEnumerated) lst 
  where
    lst = last xs
    restEnumerated = zip (take (length xs -1) xs) [1 ..]