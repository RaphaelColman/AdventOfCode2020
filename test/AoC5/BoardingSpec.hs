module AoC5.BoardingSpec(spec) where


import Test.Hspec
import AoC5.Boarding

spec :: Spec
spec = do
    describe "Binary Space Partition test" $ 
        it "can find row" $ do
            binarySpacePartition [DOWN, UP, DOWN, UP, UP, DOWN, DOWN] 127 `shouldBe` 44
            binarySpacePartition [UP, DOWN, DOWN, DOWN, UP, UP, DOWN] 127 `shouldBe` 70

    describe "Read boarding pass test" $ do
        it "can read boarding pass" $ do
            readBoardingPass "FBFBBFFRLR" `shouldBe` Just (44, 5)
            readBoardingPass "BFFFBBFRRR" `shouldBe` Just (70, 7)
            readBoardingPass "FFFBBBFRRR" `shouldBe` Just (14, 7)
            readBoardingPass "BBFFBBFRLL" `shouldBe` Just (102, 4)

        it "can get seat id" $ do
            seatId "BFFFBBFRRR" `shouldBe` Just 567
            seatId "FFFBBBFRRR" `shouldBe` Just 119
            seatId "BBFFBBFRLL" `shouldBe` Just 820