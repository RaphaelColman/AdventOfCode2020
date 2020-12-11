module AoC11.Seating where

import Common.Utils
import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Linear.V2
import Linear.Vector
import System.IO

aoc11 :: IO ()
aoc11 = do
  contents <- getInputFile 11
  let seatingArea = parseToMap contents
  --print $ runUntilStable seatingArea
  print $ runUntilStableNew seatingArea
  print "done"

type SeatingArea = M.Map (V2 Int) Char

parseToMap :: String -> SeatingArea
parseToMap s = M.fromList $ map (\((x, y), c) -> (V2 x y, c)) $ enumerateMultilineString s

allAdjacents :: V2 Int -> [V2 Int]
allAdjacents v =
  [ v + unit _x,
    v - unit _x,
    v + unit _y,
    v - unit _y,
    v + unit _x + unit _y,
    v + unit _x - unit _y,
    v - unit _x + unit _y,
    v - unit _x - unit _y
  ]

step :: SeatingArea -> SeatingArea
step sa = M.mapWithKey foo sa
  where
    foo coord value = convertSeat value [M.findWithDefault '.' c sa | c <- allAdjacents coord]

convertSeat :: Char -> String -> Char
convertSeat currentValue adjacents
  | currentValue == 'L' =
    if all (\c -> c == 'L' || c == '.') adjacents
      then '#'
      else currentValue
  | currentValue == '#' =
    if length (filter (== '#') adjacents) >= 4
      then 'L'
      else currentValue
  | otherwise = currentValue

runUntilStable :: SeatingArea -> Int
runUntilStable = go 0
  where
    go numSteps seatingArea
      | nextSeatingArea == seatingArea = length $ M.filter (== '#') seatingArea
      | otherwise = go (numSteps + 1) nextSeatingArea
      where
        nextSeatingArea = step seatingArea

runInteractive :: SeatingArea -> IO ()
runInteractive sa = do
  putStr $ renderVectorMap sa
  _ <- getLine
  runInteractive (stepNew sa)

runUntilStableNew :: SeatingArea -> Int
runUntilStableNew = go 0
  where
    go numSteps seatingArea
      | nextSeatingArea == seatingArea = length $ M.filter (== '#') seatingArea
      | otherwise = go (numSteps + 1) nextSeatingArea
      where
        nextSeatingArea = stepNew seatingArea


stepNew :: SeatingArea -> SeatingArea
stepNew sa = M.mapWithKey foo sa
    where foo coord value = convertSeatNew value $ visibleOccupiedSeats coord sa

convertSeatNew :: Char -> [V2 Int] -> Char
convertSeatNew current occupied
    | current == 'L' = if null occupied then '#' else current
    | current == '#' = if length occupied >= 5 then 'L' else current
    | otherwise = current

allCoordsInDirection :: V2 Int -> SeatingArea -> V2 Int -> [V2 Int]
allCoordsInDirection coord sa direction = unfoldr (\v -> let next = v + direction in if inBounds next sa then Just (next, next) else Nothing) coord

numberOfVisibleOccupiedSeats :: SeatingArea -> V2 Int -> Int
numberOfVisibleOccupiedSeats sa coord = length $ filter (=='#') $ map findNextSeatInDirection allDirections
    where findNextSeatInDirection direction = undefined
  
travelToSeatOrEdgeInDirection :: SeatingArea -> V2 Int -> Char
travelToSeatOrEdgeInDirection sa start = undefined

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

visibleOccupiedSeats :: V2 Int -> SeatingArea -> [V2 Int]
visibleOccupiedSeats coord sa = filter (isOccupied sa) $ nearestVisibleSeats coord sa


nearestVisibleSeats :: V2 Int -> SeatingArea -> [V2 Int]
nearestVisibleSeats coord sa = mapMaybe (find (isSeat sa) . allCoordsInDirection coord sa) allDirections

isOccupied :: SeatingArea -> V2 Int -> Bool
isOccupied sa coord =
  let seat = M.findWithDefault '.' coord sa
   in seat == '#'

isSeat :: SeatingArea -> V2 Int -> Bool
isSeat sa coord =
  let seat = M.findWithDefault '.' coord sa
   in seat == '#' || seat == 'L'

inBounds :: V2 Int -> SeatingArea -> Bool
inBounds (V2 x y) sa = and [xMin <= x, x <= xMax, yMin <= y, y <= yMax]
  where
    keys = M.keys sa
    xMax = maximumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    xMin = minimumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    yMax = maximumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    yMin = minimumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
