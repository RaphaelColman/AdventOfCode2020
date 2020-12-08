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
    | otherwise = step mem >>= runMemory

step :: Memory -> Maybe Memory
step (Mem position' instructions' accumulator visited) = do
    (instr, number) <- Seq.lookup position' instructions'
    let newVisited = S.insert position' visited
    case instr of
        "jmp" -> Just $ Mem (position' + number) instructions' accumulator newVisited
        "nop" -> Just $ Mem (position' + 1) instructions' accumulator newVisited
        "acc" -> Just $ Mem (position' + 1) instructions' (accumulator + number) newVisited
