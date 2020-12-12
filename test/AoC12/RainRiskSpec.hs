module AoC12.RainRiskSpec(spec) where

import Test.Hspec
import AoC12.RainRisk
import Common.Utils
import Text.Format
import System.Directory

spec :: Spec
spec = describe "Part 1 test" $
        it "Should do part 1" $
            part1Result `shouldReturn` 757


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
