module AoC5.Boarding where

import qualified Data.Map  as M
import           Data.Sort
import           System.IO (IOMode (ReadMode), hGetContents, openFile)


main :: IO ()
main = do
  handle <- openFile "src/AoC5/input.txt" ReadMode
  contents <- hGetContents handle
  let ids = map seatId $ lines contents
  print $ maximum ids
  print $ sequence ids >>= findMissingRow

findMissingRow :: [Int] -> Maybe Int
findMissingRow (initial:rest) = fst $ foldl go (Nothing, initial) (sort rest)
    where go (Just x, _) current = (Just x, current)
          go (Nothing, prev) current =
              if current - prev == 2
                  then (Just (current-1), current)
                  else (Nothing, current)

seatId :: String -> Maybe Int
seatId str = do
    (row, column) <- readBoardingPass str
    pure $ row * 8 + column


readBoardingPass :: String -> Maybe (Int, Int)
readBoardingPass pass = do
    (rowInstructions, columnInstructions) <- splitAt 7 <$> parseBoardingPass pass
    let row = binarySpacePartition rowInstructions 127
    let column = binarySpacePartition columnInstructions 7
    pure (row, column)

binarySpacePartition :: [Instruction] -> Int -> Int
binarySpacePartition instr = go instr 0
    where go :: [Instruction] -> Int -> Int -> Int
          go [instruction] lowerBound upperBound = if instruction == DOWN then lowerBound else upperBound
          go (current:rest) lowerBound upperBound
            | lowerBound == upperBound = lowerBound
            | otherwise = let midPoint = (lowerBound + upperBound) `div` 2 in
                if current == DOWN
                then go rest lowerBound midPoint
                else go rest (midPoint+1) upperBound

data Instruction = UP | DOWN deriving (Eq, Enum, Show)

instructionMapping :: M.Map Char Instruction
instructionMapping = M.fromList [('F', DOWN), ('B', UP), ('R', UP), ('L', DOWN)]

parseBoardingPass :: String -> Maybe [Instruction]
parseBoardingPass = traverse (`M.lookup` instructionMapping)
