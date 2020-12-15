module AoC15.RambunctiousRecitation where

import qualified Data.IntMap as IM

aoc15 :: IO ()
aoc15 = do
    print $ recite input 2020
    print $ recite2020 input 30000000

input :: [Int]
input = [18,8,0,5,4,1,20]

recite2020 :: [Int] -> Int -> Int
recite2020 ints goal = lastEntered $ recite ints goal

recite :: [Int] -> Int -> Params
recite ints goal = reciteRecursive $ PS toIntMap (length ints + 1) (last ints) (reverse ints) goal
    where toIntMap = IM.fromList $ zip ints [[x] | x <- [1..]]
data Params = PS {
    indexMap :: IM.IntMap [Int],
    count :: Int,
    lastEntered :: Int,
    ll :: [Int],
    goal :: Int
} deriving (Eq, Show)

reciteRecursive :: Params -> Params
reciteRecursive params@(PS indexMap' count' lastEntered' ll goal')
    | count' == goal' + 1 = params
    | otherwise = reciteRecursive $ PS newMap (count'+1) numberToInsert (numberToInsert:ll) goal'
    where indexes = IM.findWithDefault [] lastEntered' indexMap'
          numberToInsert = if length indexes >= 2 then let a:b:_ = indexes in a - b else 0
          valueToInsert = let existing = IM.findWithDefault [] numberToInsert indexMap' in case existing of
                                                                                            (a:b:_) -> [count',a]
                                                                                            [a] -> [count',a]
                                                                                            [] -> [count']
          newMap = IM.insert numberToInsert valueToInsert indexMap'


toIndexMap :: [Int] -> IM.IntMap [Int]
toIndexMap ints = IM.fromList $ zip ints [[x] | x <- [1..]]