{-# LANGUAGE ScopedTypeVariables #-}
module AoC12.RainRisk where

import Common.Utils
import Linear.V2
import Linear.Vector
import Prelude hiding (Right, Left)

aoc12 :: IO ()
aoc12 = do
    contents <- getInputFile 12
    let instructions = parseContents contents
    let finished = run initShip instructions
    print $ manhattanDistance finished

type Instruction = (Char, Int)

data Direction = North | East | South | West deriving (Eq, Show, Enum, Bounded)
data Turn = Right | Left deriving (Eq, Show, Enum, Bounded)
data Vessel = Ship {
    location :: V2 Int,
    facing :: Direction
} deriving (Show, Eq)

parseContents :: String -> [Instruction]
parseContents = map toInstruction . lines
    where toInstruction (i:amount) = (i, read amount)

initShip :: Vessel
initShip = Ship (V2 0 0) East

run :: Vessel -> [Instruction] -> Vessel
run = foldl step

step :: Vessel -> Instruction -> Vessel
step ves@(Ship _ facing) (instr, amount) = case instr of
    'N' -> moveCardinal ves North amount
    'E' -> moveCardinal ves East amount
    'S' -> moveCardinal ves South amount
    'W' -> moveCardinal ves West amount
    'L' -> turnVessel ves Left amount
    'R' -> turnVessel ves Right amount
    'F' -> moveCardinal ves facing amount
    _ -> undefined

turnVessel :: Vessel -> Turn -> Int -> Vessel
turnVessel (Ship location facing) turn amount = Ship location newDirection
    where rotation = case turn of
            Right -> enumNext
            Left -> enumPrev
          turns = amount `div` 90
          newDirection = last $ take turns $ tail $ iterate rotation facing

moveCardinal :: Vessel -> Direction -> Int -> Vessel
moveCardinal (Ship location facing) direction amount = Ship (location + amount *^ vector) facing
    where vector :: V2 Int = case direction of
                North -> unit _y
                East -> unit _x
                South -> - unit _y
                West -> - unit _x

manhattanDistance :: Vessel -> Int
manhattanDistance = sum . abs . location