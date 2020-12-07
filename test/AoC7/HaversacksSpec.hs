module AoC7.HaversacksSpec(spec) where

import Test.Hspec
import AoC7.Haversacks
import Common.Utils
import Text.Format
import System.Directory

spec :: Spec
spec = 
    describe "IO tests" $ do
        it "calculates containing bags" $
            containingBags `shouldReturn` 4
        
        it "calculates child bags" $
            childBags `shouldReturn` 32
            

containingBags :: IO Int
containingBags = do
    bagRules' <- bagRules
    let bagToFind = BG "shiny" "gold"
    pure $ length $ recursiveContainersForBag bagToFind bagRules'

childBags :: IO Int
childBags = do
    bagRules' <- bagRules
    let bagToFind = BG "shiny" "gold"
    pure $ numberOfBagsForBag bagToFind bagRules'

bagRules :: IO BagRules
bagRules = do
    currentDirectory <- getCurrentDirectory
    let filePath = format "{0}/test/res/AoC7/input.txt" [currentDirectory]
    contents <- readFileToString filePath
    pure $ parseInput contents