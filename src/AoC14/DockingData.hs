module AoC14.DockingData where

import Common.Utils
import Data.Bits
import qualified Data.IntMap as IM
import Data.List.Split
import Data.Char
import Data.Monoid

aoc14 :: IO ()
aoc14 = do
  contents <- getInputFile 14
  print $ part1 contents

type Mask = [Int -> Int]

type Memory = IM.IntMap Int

data MemoryState = MS
  { memory :: Memory,
    currentMask :: String
  }
  deriving (Eq, Show)

data WriteOperation = WO {
    location :: Int,
    number :: Int
  } deriving (Show, Eq)

part1 :: String -> Sum Int
part1 str = IM.foldMapWithKey (\_ v -> Sum v) mem'
    where MS mem' mask' = foldl stepMemoryState initMemoryState $ lines str

initMemoryState :: MemoryState
initMemoryState = MS IM.empty "" --This will break if the first line is not a mask

parseMaskline :: String -> Mask
parseMaskline str = let [_, _, maskString] = words str in readMaskString maskString

parseWOLine :: String -> WriteOperation
parseWOLine str = WO (toNumber memString) (toNumber numString)
    where [memString, numString] = splitOn " = " str
          toNumber  = read . filter isDigit


readMaskString :: String -> Mask
readMaskString str = map parseCharacter enumerated
  where
    enumerated = zip (reverse str) [0 ..]
    parseCharacter (ch, index)
      | ch == 'X' = id
      | ch == '1' = flip setBit index
      | ch == '0' = flip clearBit index

applyMask :: Mask -> Int -> Int
applyMask mask int = foldl (\a m -> m a) int mask

stepMemoryState :: MemoryState -> String -> MemoryState
stepMemoryState memState@(MS memory' _) str
  | take 4 str == "mask" = MS memory' str
  | take 3 str == "mem" = applyWO memState $ parseWOLine str

applyWO :: MemoryState -> WriteOperation -> MemoryState
applyWO (MS memory' currentMask') (WO location' number') = MS newMemory currentMask'
    where mask = parseMaskline currentMask' --this will reread the mask every time.
          numberToWrite = applyMask mask number'
          newMemory = IM.insert location' numberToWrite memory'