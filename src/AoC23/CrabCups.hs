{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AoC23.CrabCups where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

aoc23 :: IO ()
aoc23 = do
  let params = initParams testInput
  stepInteractive params
  print "Done"

type Cups = IntMap Int
data Params = MkParams {
  focus :: Int,
  cups :: Cups
} deriving (Eq, Ord)

instance Show Params where
  show params@(MkParams focus cups) = show focus ++ show cupList
    where cupList = takeWhile (/=focus) $ iterate (cups IM.!) (cups IM.! focus)

stepInteractive :: Params -> IO ()
stepInteractive params = do
  print params
  _ <- getLine
  stepInteractive $ step params

play :: Int -> Params -> Params
play times params = iterate step params !! times

step :: Params -> Params
step params@(MkParams focus cups) = MkParams newFocus next3Modified
  where next1 = cups IM.! focus
        next2 = cups IM.! next1
        next3 = cups IM.! next2
        next4 = cups IM.! next3
        focusModified = IM.insert focus next4 cups
        target = until (\x -> x `notElem` [next1, next2, next3])
                (\x -> 
                  if x <= 1 then 9 else x - 1
                  ) 
                (focus - 1)
        targetNext = cups IM.! target
        targetModified = IM.insert target next1 focusModified
        next3Modified = IM.insert next3 targetNext targetModified
        newFocus = next3Modified IM.! focus

---Looping logic is still broken!

makeCups :: [Int] -> Cups
makeCups xs@(start:rest) = res
  where res = IM.fromSet mkcup $ IS.fromList xs
        nexts = IM.fromList $ zip xs rest
        max' = maximum xs --Might need to hardcode this
        mkcup val = IM.findWithDefault start val nexts

initParams :: [Int] -> Params
initParams xs@(start:_) = MkParams start $ makeCups xs

testInput :: [Int]
testInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]

puzzleInput :: [Int]
puzzleInput = [3, 2, 7, 4, 6, 5, 1, 8, 9]

debugInput :: [Int]
debugInput = [2,8,9,1,5,4,6,7,3]