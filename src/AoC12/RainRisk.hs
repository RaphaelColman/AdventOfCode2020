{-# LANGUAGE ScopedTypeVariables #-}
module AoC12.RainRisk where

import           Common.Utils
import           Linear.V2
import           Linear.Vector
import           Prelude       hiding (Left, Right)

aoc12 :: IO ()
aoc12 = do
    contents <- getInputFile 12
    let instructions = parseContents contents
    print $ part1 instructions
    print $ part2 instructions

type Instruction = (Char, Int)

data Direction = North | East | South | West deriving (Eq, Show, Enum, Bounded)
data Turn = Right | Left deriving (Eq, Show, Enum, Bounded)
data Vessel = Ship {
    location :: V2 Int,
    facing   :: Direction
} | WaypointShip {
    location :: V2 Int,
    waypoint :: V2 Int
} deriving (Show, Eq)

part1 :: [Instruction] -> Int
part1 = manhattanDistance . run initShip

part2 :: [Instruction] -> Int
part2 = manhattanDistance . run initWaypointShip

parseContents :: String -> [Instruction]
parseContents = map toInstruction . lines
    where toInstruction (i:amount) = (i, read amount)

initShip :: Vessel
initShip = Ship (V2 0 0) East

initWaypointShip :: Vessel
initWaypointShip = WaypointShip (V2 0 0) (V2 10 1)

run :: Vessel -> [Instruction] -> Vessel
run = foldl step

step :: Vessel -> Instruction -> Vessel
step ves (instr, amount) = case instr of
    'N' -> moveCardinal ves North amount
    'E' -> moveCardinal ves East amount
    'S' -> moveCardinal ves South amount
    'W' -> moveCardinal ves West amount
    'L' -> turn ves Left amount
    'R' -> turn ves Right amount
    'F' -> moveFoward ves amount
    _   -> undefined

moveFoward :: Vessel -> Int -> Vessel
moveFoward vessel amount = case vessel of
    (Ship _ facing) -> moveCardinal vessel facing amount
    (WaypointShip location waypoint) -> WaypointShip (location + amount *^ waypoint) waypoint

moveCardinal :: Vessel -> Direction -> Int -> Vessel
moveCardinal ship direction amount = case ship of
    (Ship location facing) -> Ship (location + amount *^ vector) facing
    (WaypointShip location waypoint) -> WaypointShip location (waypoint + amount *^ vector)
    where vector :: V2 Int = case direction of
                North -> unit _y
                East  -> unit _x
                South -> - unit _y
                West  -> - unit _x

turn :: Vessel -> Turn -> Int -> Vessel
turn vessel turn amount = case vessel of
    (Ship _ _)         -> turnVessel vessel turn amount
    (WaypointShip _ _) -> rotateWaypoint vessel turn amount

rotateWaypoint :: Vessel -> Turn -> Int -> Vessel
rotateWaypoint (WaypointShip location waypoint) direction amount = WaypointShip location newWaypoint
    where turns = amount `div` 90
          newWaypoint = last $ take turns $ tail $ iterate (rotateVector direction) waypoint

turnVessel :: Vessel -> Turn -> Int -> Vessel
turnVessel (Ship location facing) turn amount = Ship location newDirection
    where rotation = case turn of
            Right -> enumNext
            Left  -> enumPrev
          turns = amount `div` 90
          newDirection = last $ take turns $ tail $ iterate rotation facing

rotateVector :: Turn -> V2 Int -> V2 Int
rotateVector turn (V2 x y) = case turn of
    Right -> V2 y (-x)
    Left  -> V2 (-y) x

manhattanDistance :: Vessel -> Int
manhattanDistance = sum . abs . location
