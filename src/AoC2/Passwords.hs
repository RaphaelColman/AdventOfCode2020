{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AoC2.Passwords where

import Data.List.Split
import qualified Data.Map as M
import Prelude hiding (max, min)
import Text.Read ( readMaybe )
import Common.Utils

data Rule = FrequencyRule
  { password :: Password,
    min :: Int,
    max :: Int,
    letter :: Char
  }
  deriving (Show, Eq)

type Password = String

aoc2 :: IO ()
aoc2 = do
  contents <- getInputFile 2
  let thing = parseContentsMaybe contents
  let satisfiesRuleCount = length . filter satisfiesRule <$> thing 
  let satisfiesPositionRuleCount = length . filter satisfiesPositionRule <$> thing 
  print satisfiesRuleCount
  print satisfiesPositionRuleCount

parseContentsMaybe :: String -> Maybe [Rule]
parseContentsMaybe = traverse parseLineMaybe . lines

parseLineMaybe :: String -> Maybe Rule
parseLineMaybe line = do
  [minMax, ltr:_, pwd] <- pure $ words line
  [mn, mx] <- pure $ splitOn "-" minMax
  FrequencyRule pwd <$> readMaybe mn <*> readMaybe mx <*> pure ltr

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