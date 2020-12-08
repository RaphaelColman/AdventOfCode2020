module AoC8.Handheld where

import Common.Utils
import qualified Data.Sequence as Seq hiding (filter)
import qualified Data.Set as S

aoc8 :: IO ()
aoc8 = do
  contents <- getInputFile 8
  let memory = initMemory $ parseInstructions contents
  let result = runMemory memory
  print $ accumulator <$> result
  let part2Result = findSuccessfulFlip memory
  print $ accumulator <$> part2Result

type Instruction = (String, Int)

data Memory = Mem {
    position :: Int,
    instructions :: Seq.Seq Instruction,
    accumulator :: Int,
    indexesVisited :: S.Set Int
} deriving (Show, Eq)

parseInstructions :: String -> Seq.Seq Instruction
parseInstructions = Seq.fromList . map toInstruction . lines . filter (/= '+')
  where
    toInstruction ln = let [instruction', number] = words ln in (instruction', read number)

initMemory :: Seq.Seq Instruction -> Memory
initMemory instr = Mem 0 instr 0 S.empty

runMemory :: Memory -> Maybe Memory
runMemory mem@(Mem position' _ _ visited)
    | S.member position' visited = Just mem
    | terminatedSuccessfully mem = Just mem
    | otherwise = step mem >>= runMemory

findSuccessfulFlip :: Memory -> Maybe Memory
findSuccessfulFlip mem = go 0 mem
    where go indexToFlip mem' = do
            flipped <- flipInstruction mem' indexToFlip
            result <- runMemory flipped
            if terminatedSuccessfully result
                then pure result
                else go (indexToFlip + 1) mem

step :: Memory -> Maybe Memory
step (Mem position' instructions' accumulator visited) = do
    (instr, number) <- Seq.lookup position' instructions'
    let newVisited = S.insert position' visited
    case instr of
        "jmp" -> Just $ Mem (position' + number) instructions' accumulator newVisited
        "nop" -> Just $ Mem (position' + 1) instructions' accumulator newVisited
        "acc" -> Just $ Mem (position' + 1) instructions' (accumulator + number) newVisited

terminatedSuccessfully :: Memory -> Bool
terminatedSuccessfully (Mem position' instructions' _ _) = position' >= length instructions'

flipInstruction :: Memory -> Int -> Maybe Memory
flipInstruction (Mem position' instructions' accumulator visited) index = do
    (instr, amount) <- Seq.lookup index instructions'
    let newInstructions = case instr of 
            "jmp" -> Seq.update index ("nop", amount) instructions'
            "nop" -> Seq.update index ("jmp", amount) instructions'
            _ -> instructions'
    pure $ Mem position' newInstructions accumulator visited