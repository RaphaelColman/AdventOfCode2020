module AoC12.RainRiskSpec(spec) where

import Test.Hspec
import AoC12.RainRisk
import Common.Utils
import Text.Format
import System.Directory

spec :: Spec
spec = describe "E2E Test" $ do
        it "Should do part 1" $
            part1Result `shouldReturn` 757
        
        it "Should do part 2" $
            part2Result `shouldReturn` 51249


readInput :: IO String
readInput = do
    currentDirectory <- getCurrentDirectory
    let filePath = format "{0}/test/res/AoC12/input.txt" [currentDirectory]
    readFileToString filePath

part1Result :: IO Int
part1Result = do
    contents <- readInput
    let instructions = parseContents contents
    pure $ part1 instructions

part2Result :: IO Int
part2Result = do
    contents <- readInput
    let instructions = parseContents contents
    pure $ part2 instructions
