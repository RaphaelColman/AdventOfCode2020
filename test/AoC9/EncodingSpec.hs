module AoC9.EncodingSpec(spec) where

import Test.Hspec
import AoC9.Encoding

spec :: Spec
spec = do
    describe "Preamble test" $ do
        it "can identify summable from list" $ do
            summableFromList 40 [35, 20, 15, 25, 47] `shouldBe` True
            summableFromList 62 [20, 15, 25, 47, 40] `shouldBe` True
            summableFromList 182 [65, 95, 102, 117, 150] `shouldBe` True
            summableFromList 127 [95, 102, 117, 150, 182] `shouldBe` False


--[35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182]