module AoC15.RambunctiousRecitationSpec(spec) where

import Test.Hspec
import AoC15.RambunctiousRecitation

spec :: Spec
spec =
    describe "Part 1 test" $ do
        it "can do part 1" $ do
            recite2020 [1,3,2] 2020 `shouldBe` 1
            recite2020 [2,1,3] 2020 `shouldBe` 10
            recite2020 [1,2,3] 2020 `shouldBe` 27
            recite2020 [2,3,1] 2020 `shouldBe` 78
            recite2020 [3,2,1] 2020 `shouldBe` 438
            recite2020 [3,1,2] 2020 `shouldBe` 1836

        it "can do part 2" $ do
            recite2020 [1,3,2] 30000000 `shouldBe` 2578
            recite2020 [2,1,3] 30000000 `shouldBe` 3544142
            recite2020 [1,2,3] 30000000 `shouldBe` 261214
            recite2020 [2,3,1] 30000000 `shouldBe` 6895259
            recite2020 [3,2,1] 30000000 `shouldBe` 18
            recite2020 [3,1,2] 30000000 `shouldBe` 362
