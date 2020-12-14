module AoC14.DockingData where

import Common.Utils
import Data.Bits
import Data.Char
import Data.Char (digitToInt)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.List.Split
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid
import Debug.Trace
import Numeric (readInt)

aoc14 :: IO ()
aoc14 = do
  contents <- getInputFile 14
  --print $ part1 contents
  print $ part2 contents

type Mask = [Int -> Int]

type Memory = IM.IntMap Int

data MemoryState = MS
  { memory :: Memory,
    currentMask :: String
  }
  deriving (Eq, Show)

data WriteOperation = WO
  { location :: Int,
    number :: Int
  }
  deriving (Show, Eq)

part1 :: String -> Sum Int
part1 =
  IM.foldMapWithKey (\_ v -> Sum v)
    . memory
    . foldl stepMemoryState initMemoryState
    . lines

part2 :: String -> Sum Int
part2 =
  IM.foldMapWithKey (\_ v -> Sum v)
    . memory
    . foldl stepMemoryState2 initMemoryState
    . lines

initMemoryState :: MemoryState
initMemoryState = MS IM.empty "" --This will break if the first line is not a mask

parseMaskline :: String -> Mask
parseMaskline str = let [_, _, maskString] = words str in readMaskString maskString

parseWOLine :: String -> WriteOperation
parseWOLine str = WO (toNumber memString) (toNumber numString)
  where
    [memString, numString] = splitOn " = " str
    toNumber = read . filter isDigit

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
  where
    mask = parseMaskline currentMask' --this will reread the mask every time.
    numberToWrite = applyMask mask number'
    newMemory = IM.insert location' numberToWrite memory'

parseMaskToString :: String -> String
parseMaskToString str = let [_, _, maskString] = words str in maskString

stepMemoryState2 :: MemoryState -> String -> MemoryState
stepMemoryState2 memState@(MS memory' _) str
  | take 4 str == "mask" = MS memory' (parseMaskToString str)
  | take 3 str == "mem" = applyWOV2 memState $ parseWOLine str

applyWOV2 :: MemoryState -> WriteOperation -> MemoryState
applyWOV2 (MS memory' currentMask') (WO location' number') = MS newMemory currentMask'
  where
    locations = applyV2Mask currentMask' location'
    newMemory = foldl (\mem l -> IM.insert l number' mem) memory' locations

applyV2Mask :: String -> Int -> [Int]
applyV2Mask str int = applyFloatingBits floatingBits applyOnes
  where
    justOnes = map (\c -> if c == 'X' then '0' else c) str --Breaks if mask invalid. Consider ditching fromJust
    justOnesBinary = fromJust $ readBin justOnes
    applyOnes = int .|. justOnesBinary
    floatingBits = elemIndices 'X' $ reverse str

applyFloatingBit :: Int -> [Int] -> [Int] --Consider using intsets
applyFloatingBit index = concatMap (\int -> [setBit int index, clearBit int index])

applyFloatingBits :: [Int] -> Int -> [Int]
applyFloatingBits indexes int = foldl (flip applyFloatingBit) [int] indexes

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt