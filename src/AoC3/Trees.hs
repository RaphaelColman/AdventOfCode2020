module AoC3.Trees where

import qualified Data.Set as S
import Linear.V2
import System.IO
import Common.Utils

type TreeSet = S.Set (V2 Int)

data TreeMap = TM
  { _treeSet :: TreeSet,
    _width :: Int,
    _height :: Int
  } deriving (Show, Eq)

main :: IO ()
main = do
  handle <- openFile "src/AoC3/input.txt" ReadMode
  contents <- hGetContents handle
  let tm = parseContents contents
  print $ numberOfTrees tm (V2 3 1)
  print $ numberOfTreesForSlopes tm [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]

parseContents :: String -> TreeMap
parseContents contents = TM ts width height
  where
    enumerated = enumerateMultilineString contents
    justTrees = filter (\(_, c) -> c == '#') enumerated
    ts = S.fromList $ map (\((x, y), _) -> V2 x y) justTrees
    width = (+) 1 $ maximum $ map (\((x, _), _) -> x) enumerated
    height = (+) 1 $ maximum $ map (\((_, y), _) -> y) enumerated --there is probably a better way of doing this

isTree :: V2 Int -> TreeMap -> Bool
isTree (V2 x y) (TM ts width _) = S.member wrappedCoord ts
  where
    wrappedCoord = V2 (x `mod` width) y

makeCoords :: Int -> V2 Int -> [V2 Int]
makeCoords maxY vector = takeWhile (\(V2 _ y) -> y <= maxY) $ scanl (+) (V2 0 0) $ repeat vector

numberOfTrees :: TreeMap -> V2 Int -> Int
numberOfTrees tm@(TM _ _ height) vector = length justTrees
  where
    coords = makeCoords height vector
    justTrees = filter (`isTree` tm) coords

numberOfTreesForSlopes :: TreeMap -> [V2 Int] -> Int
numberOfTreesForSlopes tm = product . map (numberOfTrees tm)
