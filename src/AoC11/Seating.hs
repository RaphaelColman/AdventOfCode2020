module AoC11.Seating where

import Common.Utils
import qualified Data.Map as M
import Data.Maybe
import Linear.V2
import Linear.Vector

aoc11 :: IO ()
aoc11 = do
  contents <- getInputFile 11
  let seatingArea = parseToMap contents
  print $ runUntilStable (RP numberOfOccupiedAdjacentSeats 4) seatingArea
  print $ runUntilStable (RP numberOfVisibleOccupiedSeats 5) seatingArea

type SeatingArea = M.Map (V2 Int) Char

data RunParameters = RP
  { numberOfOccupiedSeatsFunction :: SeatingArea -> V2 Int -> Int,
    threshold :: Int
  }

parseToMap :: String -> SeatingArea
parseToMap s = M.fromList $ map (\((x, y), c) -> (V2 x y, c)) $ enumerateMultilineString s

allAdjacents :: V2 Int -> [V2 Int]
allAdjacents v = map (v +) allDirections

stepFun :: RunParameters -> SeatingArea -> SeatingArea
stepFun (RP fun threshold) sa = M.mapWithKey (\coord value -> convertSeat threshold value (fun sa coord)) sa

numberOfVisibleOccupiedSeats :: SeatingArea -> V2 Int -> Int
numberOfVisibleOccupiedSeats sa coord = length $ filter (== '#') $ mapMaybe (travelToSeatOrEdgeInDirection sa coord) allDirections

numberOfOccupiedAdjacentSeats :: SeatingArea -> V2 Int -> Int
numberOfOccupiedAdjacentSeats sa = length . filter (== '#') . mapMaybe (`M.lookup` sa) . allAdjacents

convertSeat :: Int -> Char -> Int -> Char
convertSeat threshold current occupied
  | current == 'L' = if occupied == 0 then '#' else current
  | current == '#' = if occupied >= threshold then 'L' else current
  | otherwise = current

runUntilStable :: RunParameters -> SeatingArea -> Int
runUntilStable rp = go 0
  where
    go numSteps seatingArea
      | nextSeatingArea == seatingArea = length $ M.filter (== '#') seatingArea
      | otherwise = go (numSteps + 1) nextSeatingArea
      where
        nextSeatingArea = stepFun rp seatingArea

travelToSeatOrEdgeInDirection :: SeatingArea -> V2 Int -> V2 Int -> Maybe Char
travelToSeatOrEdgeInDirection sa = go
  where
    go start direction = case found of
      Just ch -> if ch `elem` "L#" then found else go next direction
      Nothing -> found
      where
        next = start + direction
        found = M.lookup next sa

allDirections :: [V2 Int]
allDirections =
  [ unit _x,
    - unit _x,
    unit _y,
    - unit _y,
    unit _x + unit _y,
    unit _x - unit _y,
    - unit _x + unit _y,
    - unit _x - unit _y
  ]
