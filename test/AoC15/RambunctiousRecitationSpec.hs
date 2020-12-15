module AoC15.RambunctiousRecitationSpec(spec) where

import Test.Hspec
import AoC15.RambunctiousRecitation

spec :: Spec
spec =
    describe "Part 1 test" $ do
        it "can do part 1" $ do
            recite [1,3,2] 2020 `shouldBe` 1
            recite [2,1,3] 2020 `shouldBe` 10
            recite [1,2,3] 2020 `shouldBe` 27
            recite [2,3,1] 2020 `shouldBe` 78
            recite [3,2,1] 2020 `shouldBe` 438
            recite [3,1,2] 2020 `shouldBe` 1836

        xit "can do part 2" $ do
            recite [1,3,2] 30000000 `shouldBe` 2578
            recite [2,1,3] 30000000 `shouldBe` 3544142
            recite [1,2,3] 30000000 `shouldBe` 261214
            recite [2,3,1] 30000000 `shouldBe` 6895259
            recite [3,2,1] 30000000 `shouldBe` 18
            recite [3,1,2] 30000000 `shouldBe` 362
