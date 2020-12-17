{-# LANGUAGE ScopedTypeVariables #-}
module AoC17.ConwayCubes where

import Common.Utils
import Linear.V3
import Linear.Vector
import qualified Data.Map as M
import Control.Lens
import Linear.V4

aoc17 :: IO ()
aoc17 = do
    contents <- getInputFile 17
    let cubeSpace = parseToCubeSpace contents
    print $ part1 cubeSpace
    let hyperCubeSpace = parseToHyperCubeSpace contents
    print $ part2 hyperCubeSpace

type Coordinate = V3 Int
type CubeSpace = M.Map Coordinate Bool

type HyperCoord = V4 Int
type HyperCubeSpace = M.Map HyperCoord Bool

part1 :: CubeSpace -> Int
part1 cubeSpace = activeCubes $ iterate stepCubeSpace cubeSpace !! 6

part2 :: HyperCubeSpace -> Int
part2 hyperCubeSpace = length $ M.filter id $ iterate hyperStepCubeSpace hyperCubeSpace !! 6


parseToCubeSpace :: String -> CubeSpace
parseToCubeSpace s = M.fromList $ map (\((x, y), c) -> (V3 x y 0, c == '#')) $ enumerateMultilineString s

parseToHyperCubeSpace :: String -> HyperCubeSpace
parseToHyperCubeSpace s = M.fromList $ map (\((x, y), c) -> (V4 x y 0 0, c == '#')) $ enumerateMultilineString s

neighbours :: Coordinate -> [Coordinate]
neighbours coord = map (+ coord) directions
    where directions :: [V3 Int] = [ V3 x y z | x <- units, y <- units, z <- units, [x,y,z] /= [0,0,0]]
          units = [-1,0,1]

hyperNeighbours :: HyperCoord -> [HyperCoord]
hyperNeighbours coord = map (+ coord) directions
    where directions :: [V4 Int] = [ V4 x y z w | x <- units, y <- units, z <- units, w <- units, [x,y,z,w] /= [0,0,0,0]]
          units = [-1,0,1]

activeNeighbours :: CubeSpace -> Coordinate -> Int
activeNeighbours cubeSpace coord = length $ filter (\n -> M.findWithDefault False n cubeSpace) $ neighbours coord

hyperActiveNeighbours :: HyperCubeSpace -> HyperCoord -> Int
hyperActiveNeighbours cubeSpace coord = length $ filter (\n -> M.findWithDefault False n cubeSpace) $ hyperNeighbours coord

stepCubeSpace :: CubeSpace -> CubeSpace
stepCubeSpace cubeSpace = M.mapWithKey change expanded
    where expanded = expandCubeSpace cubeSpace
          change coord state
            | state = numActive == 3 || numActive == 2
            | not state = numActive == 3
            where numActive = activeNeighbours expanded coord

hyperStepCubeSpace :: HyperCubeSpace -> HyperCubeSpace
hyperStepCubeSpace cubeSpace = M.mapWithKey change expanded
    where expanded = hyperExpandCubeSpace cubeSpace
          change coord state
            | state = numActive == 3 || numActive == 2
            | not state = numActive == 3
            where numActive = hyperActiveNeighbours expanded coord


expandCubeSpace :: CubeSpace -> CubeSpace
expandCubeSpace cubeSpace = foldr (`M.insert` False) cubeSpace allNeighbours
    where allNeighbours = filter (`M.notMember` cubeSpace) $ concatMap neighbours $  M.keys cubeSpace

hyperExpandCubeSpace :: HyperCubeSpace -> HyperCubeSpace
hyperExpandCubeSpace cubeSpace = foldr (`M.insert` False) cubeSpace allNeighbours
    where allNeighbours = filter (`M.notMember` cubeSpace) $ concatMap hyperNeighbours $  M.keys cubeSpace


activeCubes :: CubeSpace -> Int
activeCubes = length . M.filter id 