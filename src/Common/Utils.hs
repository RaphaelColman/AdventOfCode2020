{-# LANGUAGE TupleSections #-}

module Common.Utils where

import Control.Lens
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Linear.V2
import System.Directory
import System.IO
import Text.Format

enumerateMultilineString :: String -> [((Int, Int), Char)]
enumerateMultilineString str
  | maximum lengths /= minimum lengths = error "Line lengths are not equal"
  | otherwise = zip coords (concat lines')
  where
    lines' = lines str
    xLength = length (head lines')
    yLength = length lines'
    lengths = map length lines'
    coords = [(x, y) | y <- [0 .. yLength -1], x <- [0 .. xLength - 1]]
  
enumerateMultilineStringToVectorMap :: String -> Map.Map (V2 Int) Char
enumerateMultilineStringToVectorMap = Map.fromList . map (\((x, y), c) -> (V2 x y, c)) . enumerateMultilineString

readFileToString :: String -> IO String
readFileToString filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

getInputFile :: Int -> IO String
getInputFile puzzleNumber = do
  workingDirectory <- getCurrentDirectory
  let path = format "{0}/res/AoC{1}/input.txt" [workingDirectory, show puzzleNumber]
  readFileToString path

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

freqs :: (Ord k, Num a) => [k] -> Map.Map k a
freqs xs = Map.fromListWith (+) (map (,1) xs)

renderVectorMap :: Map.Map (V2 Int) Char -> String
renderVectorMap m = foo
  where
    keys = Map.keys m
    xMax = maximumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    xMin = minimumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    yMax = maximumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    yMin = minimumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    xRange = (xMax - xMin) + 1
    panelList = [Map.findWithDefault '.' (V2 x y) m | y <- [yMin .. yMax], x <- [xMin .. xMax]]
    panelRows = chunksOf xRange panelList
    foo = unlines (replicate xRange '=' : panelRows)

enumNext :: (Enum a, Eq a, Bounded a) => a -> a
enumNext e
      | e == maxBound  = minBound
      | otherwise = succ e


enumPrev :: (Enum a, Eq a, Bounded a) => a -> a
enumPrev e
      | e == minBound = maxBound
      | otherwise = pred e


stepEnum :: (Enum a, Eq a, Bounded a) => a -> Int -> a
stepEnum enum times = iterate enumNext enum !! times

prevStepEnum :: (Enum a, Eq a, Bounded a) => a -> Int -> a
prevStepEnum enum times = iterate enumPrev enum !! times