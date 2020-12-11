module AoC11.SeatingSpec(spec) where

import Test.Hspec
import AoC11.Seating
import Common.Utils
import System.Directory

spec :: Spec
spec = do
    describe "Find visible seats" $
        it "can find the nearest visible seats" $ do
            True `shouldBe` False
