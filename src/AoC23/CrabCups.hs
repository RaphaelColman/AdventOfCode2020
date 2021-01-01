{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AoC23.CrabCups where

import Control.Lens
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List.PointedList (moveTo)
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sort
import           Data.Finite
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import GHC.TypeLits
import Control.Monad.Primitive

import Debug.Trace

aoc23 :: IO ()
aoc23 = do
  let game = initGame puzzleInput
  print $ part1 game
  let part2Game@(MkGame plc lookup) = initGame $ levelUpInput testInput
  print $ play 5 part2Game
  print "Done"

data Game = MkGame
  { pointedList :: PLC.PointedList Int,
    lookup :: IntMap Int
  }
  deriving (Eq, Show)

play :: Int -> Game -> Game
play times game = iterate step' game !! times

part1 :: Game -> String
part1 game = concatMap show orderedList
  where
    (MkGame plc lookup) = play 100 game
    (PLC.PointedList prefix focus suffix) = fromJust $ moveTo (lookup IM.! 1) plc
    orderedList = suffix ++ reverse prefix

step' :: Game -> Game
step' game = progressGame (MkGame movedBackToFocus insertedLookup)
  where
    removedGame@(MkGame removedPlc@(PLC.PointedList removedPrefix removedFocus removedSuffix) removedLookup, removedItems) = removeNextXGame 3 game
    highest = IS.findMax $ IM.keysSet removedLookup
    countdown = IS.lookupLT removedFocus $ IM.keysSet removedLookup
    destinationCup = fromMaybe highest countdown
    destinationCupIndex = removedLookup IM.! destinationCup
    moveToDestination = fromJust $ moveTo destinationCupIndex removedPlc
    inserted@(MkGame insertedPlc insertedLookup) = insertFoldableRightGame removedItems (MkGame moveToDestination removedLookup)
    movedBackToFocus = fromJust $ moveTo (insertedLookup IM.! removedFocus) insertedPlc

removeNextX :: Int -> PLC.PointedList a -> (PLC.PointedList a, Seq a)
removeNextX x plc@(PLC.PointedList prefix focus rest) = (PLC.previous list, removed)
  where
    rotate1 = PLC.next plc
    initial = (rotate1, Seq.empty)
    doRemove = iterate (\(plc', seq) -> let (newList, removed) = removeRight plc' in (newList, seq Seq.|> removed)) initial
    (list, removed) = doRemove !! x

removeRight :: PLC.PointedList a -> (PLC.PointedList a, a)
removeRight plc@(PLC.PointedList prefix focus rest) = (fromJust (PLC.delete plc), focus)

removeNextXGame :: Int -> Game -> (Game, Seq Int)
removeNextXGame x (MkGame plc@(PLC.PointedList prefix focus suffix) lookup) = (MkGame strippedList newLookup, removed)
  where
    (strippedList, removed) = removeNextX 3 plc
    lookUpWithRemovedDeleted = foldl' (flip IM.delete) lookup removed
    newLookup = foldl' updateIndex lookUpAdjustedForLoop strippedList
    takenFromStart = let len = length suffix in if len >= 3 then 0 else x - len
    lookUpAdjustedForLoop =
      if takenFromStart > 0
        then IM.map (\n -> n - takenFromStart) lookUpWithRemovedDeleted
        else lookUpWithRemovedDeleted
    updateIndex im val =
      IM.adjust
        ( \v ->
            if v > PLC.index strippedList
              then v - x
              else v
        )
        val
        im

insertFoldableRightGame :: (Foldable m) => m Int -> Game -> Game
insertFoldableRightGame foldable (MkGame plc@(PLC.PointedList prefix focus suffix) lookup) = MkGame newList withNewElems
  where
    newList = insertFoldableRight foldable plc
    focusIndex = PLC.index plc
    (withNewElems, lastAddedIndex) = foldl' adjustForInsertion (incrementForInsertion, 1) foldable
    adjustForInsertion (im, count) val = (IM.insert val (count + focusIndex) im, count + 1)
    incrementForInsertion =
      IM.map
        ( \a ->
            if a > focusIndex
              then a + length foldable
              else a
        )
        lookup

insertFoldableRight :: (Foldable m) => m a -> PLC.PointedList a -> PLC.PointedList a
insertFoldableRight foldable plc = PLC.moveN (- (length foldable)) inserted
  where
    inserted = foldl' (flip PLC.insert) plc foldable

progressGame :: Game -> Game
progressGame (MkGame plc lookup) = MkGame (PLC.next plc) lookup

initGame :: [Int] -> Game
initGame xs = MkGame plc mp
  where
    plc = fromJust $ PLC.fromList xs
    mp =
      let indexZip = zip xs [0 ..]
       in IM.fromList indexZip

testInput :: [Int]
testInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]

puzzleInput :: [Int]
puzzleInput = [3, 2, 7, 4, 6, 5, 1, 8, 9]

levelUpInput :: (Ord a, Num a, Enum a) => [a] -> [a]
levelUpInput input = input ++ rest
    where maxNum = maximum input
          rest = [maxNum + 1, maxNum+2 .. 10000000]

newtype CrabState s = CrabState { csRight  :: MV.MVector s Int }

step :: forall m s. (PrimMonad m, PrimState m ~ s) => CrabState s -> Int -> m Int
step CrabState{..} lab = do
    (gs@(g1,_,g3),lab') <- pull3 lab
    MV.unsafeWrite csRight lab lab'
    let target = until (notAny gs) subWrap (subWrap lab)
    aftertarg <- MV.unsafeRead csRight target
    MV.unsafeWrite csRight target g1
    MV.unsafeWrite csRight g3 aftertarg
    pure lab'
  where
    n = MV.length csRight
    subWrap x
      | x == 0    = n - 1
      | otherwise = x - 1
    notAny (g1,g2,g3) x = x /= g1 && x /= g2 && x /= g3
    {-# INLINE notAny #-}
    pull3 :: Int -> m ((Int, Int, Int), Int)
    pull3 i0 = do
      i1 <- MV.unsafeRead csRight i0
      i2 <- MV.unsafeRead csRight i1
      i3 <- MV.unsafeRead csRight i2
      i4 <- MV.unsafeRead csRight i3
      pure ((i1,i2,i3),i4)
    {-# INLINE pull3 #-}
{-# INLINE step #-}