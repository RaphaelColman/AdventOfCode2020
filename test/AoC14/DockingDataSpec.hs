module AoC14.DockingDataSpec(spec) where

import Test.Hspec
import AoC14.DockingData
import qualified Data.IntSet as IS

spec :: Spec
spec = 
    describe "V2 mask" $
        it "Can apply v2 mask" $ do
            IS.fromList (applyV2Mask "000000000000000000000000000000X1001X" 42) `shouldBe` IS.fromList [26, 27, 58, 59]
            IS.fromList (applyV2Mask "00000000000000000000000000000000X0XX" 26) `shouldBe` IS.fromList [16, 17, 18, 19, 24, 25, 26, 27] 

