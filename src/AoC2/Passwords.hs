{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AoC2.Passwords where

import Data.List.Split
import qualified Data.Map as M
import System.IO
import Prelude hiding (max, min)

data Rule = FrequencyRule
  { password :: Password,
    min :: Int,
    max :: Int,
    letter :: Char
  }
  deriving (Show, Eq)

type Password = String

main :: IO ()
main = do
  handle <- openFile "src/AoC2/input.txt" ReadMode
  contents <- hGetContents handle
  let rules = parseContents contents
  print $ length $ filter satisfiesRule rules
  print $ length $ filter satisfiesPositionRule rules

parseContents :: String -> [Rule]
parseContents = map parseLine . lines

parseLine :: String -> Rule
parseLine l = FrequencyRule t mn mx char
  where
    split = splitOn ": " l
    r = splitOn " " $ head split
    minMax = splitOn "-" $ head r
    (mn :: Int, mx :: Int) = (read (head minMax), read (last minMax))
    char = head $ last r
    t = last split

satisfiesRule :: Rule -> Bool
satisfiesRule rule =
  count >= min rule && count <= max rule
  where
    freqMap = freqs $ password rule
    count = M.findWithDefault 0 (letter rule) freqMap

satisfiesPositionRule :: Rule -> Bool
satisfiesPositionRule r = length (filter (\c -> pwd !! c == ch) [pos1, pos2]) == 1
  where
    pos1 = min r - 1
    pos2 = max r - 1
    ch = letter r
    pwd = password r

freqs :: (Ord k, Num a) => [k] -> M.Map k a
freqs xs = M.fromListWith (+) (map (,1) xs)