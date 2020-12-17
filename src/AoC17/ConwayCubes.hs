{-# LANGUAGE ScopedTypeVariables #-}
module AoC17.ConwayCubes where

import Common.Utils
import Linear.V3
import Linear.Vector
import qualified Data.Map as M
import Control.Lens

aoc17 :: IO ()
aoc17 = do
    contents <- getInputFile 17
    let cubeSpace = parseToCubeSpace contents
    print $ part1 cubeSpace

type Coordinate = V3 Int
type CubeSpace = M.Map Coordinate Bool

part1 :: CubeSpace -> Int
part1 cubeSpace = activeCubes $ iterate stepCubeSpace cubeSpace !! 6

parseToCubeSpace :: String -> CubeSpace
parseToCubeSpace s = M.fromList $ map (\((x, y), c) -> (V3 x y 0, c == '#')) $ enumerateMultilineString s

neighbours :: Coordinate -> [Coordinate]
neighbours coord = map (+ coord) directions
    where directions :: [V3 Int] = [ V3 x y z | x <- units, y <- units, z <- units, [x,y,z] /= [0,0,0]]
          units = [-1,0,1]

activeNeighbours :: M.Map Coordinate Bool -> Coordinate -> Int
activeNeighbours cubeSpace coord = length $ filter (\n -> M.findWithDefault False n cubeSpace) $ neighbours coord

stepCubeSpace :: CubeSpace -> CubeSpace
stepCubeSpace cubeSpace = M.mapWithKey change expanded
    where expanded = expandCubeSpace cubeSpace
          change coord state
            | state = numActive == 3 || numActive == 2
            | not state = numActive == 3
            where numActive = activeNeighbours expanded coord

expandCubeSpace :: CubeSpace -> CubeSpace
expandCubeSpace cubeSpace = foldr (`M.insert` False) cubeSpace allNeighbours
    where allNeighbours = filter (`M.notMember` cubeSpace) $ concatMap neighbours $  M.keys cubeSpace

zOnly :: Int -> CubeSpace -> CubeSpace
zOnly zValue = M.filterWithKey (\coord _ -> coord ^. _z == zValue)

activeCubes :: CubeSpace -> Int
activeCubes = length . M.filter id 