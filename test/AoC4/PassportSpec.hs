module AoC4.PassportSpec(spec) where

import Test.Hspec
import AoC4.Passport

spec :: Spec
spec = do
    describe "Birth year test" $ do
        it "must be exactly four digits" $ do
            birthyearValid "11111" `shouldBe` False
            birthyearValid "111" `shouldBe` False
            birthyearValid "1999" `shouldBe` True
        
        it "must be at least 1920 and at most 2002" $ do
            birthyearValid "1919" `shouldBe` False
            birthyearValid "1920" `shouldBe` True
            birthyearValid "2002" `shouldBe` True
            birthyearValid "2003" `shouldBe` False
    
    describe "Issue Year Test" $ do
        it "must be exactly four digits" $ do
            issueYearValid "11111" `shouldBe` False
            issueYearValid "111" `shouldBe` False
            issueYearValid "2015" `shouldBe` True
        
        it "must be at least 2010 and at most 2020" $ do
            issueYearValid "2009" `shouldBe` False
            issueYearValid "2010" `shouldBe` True
            issueYearValid "2020" `shouldBe` True
            issueYearValid "2021" `shouldBe` False

    describe "Expiration Year Test" $ do
        it "must be excactly four digits" $ do
            expiryYearValid "11111" `shouldBe` False
            expiryYearValid "11" `shouldBe` False
            expiryYearValid "2025" `shouldBe` True
        
        it "must be at least 2020 and at most 2030" $ do
            expiryYearValid "2019" `shouldBe` False
            expiryYearValid "2020" `shouldBe` True
            expiryYearValid "2030" `shouldBe` True
            expiryYearValid "2031" `shouldBe` False
    
    describe "Hair colour test" $ do
        it "must be # followed by exactly 6 characters 0-9 or a-f" $ do
            hairColourValid "#123abc" `shouldBe` True
            hairColourValid "#123abz" `shouldBe` False
            hairColourValid "123abc" `shouldBe` False

    describe "Eye colour test" $ do
        it "must be exactly one of: amb blu brn gry grn hzl oth" $ do
            eyeColourValid "amb" `shouldBe` True
            eyeColourValid "blu" `shouldBe` True
            eyeColourValid "brn" `shouldBe` True
            eyeColourValid "gry" `shouldBe` True
            eyeColourValid "grn" `shouldBe` True
            eyeColourValid "hzl" `shouldBe` True
            eyeColourValid "oth" `shouldBe` True
            eyeColourValid "wat" `shouldBe` False

    describe "Passport Id Test" $ do
        it "must be a 9 digit number including leading zeros" $ do
           passportIdValid "000000001" `shouldBe` True
           passportIdValid "0123456789" `shouldBe` False

    describe "Height test" $ do
        it "must be between 150 and 193 inclusive if cm" $ do
            heightValid "149cm" `shouldBe` False
            heightValid "150cm" `shouldBe` True
            heightValid "193cm" `shouldBe` True
            heightValid "194cm" `shouldBe` False

        it "must be Between 59 and 76 inclusive if inches" $ do
            heightValid "58in" `shouldBe` False
            heightValid "59in" `shouldBe` True
            heightValid "76in" `shouldBe` True
            heightValid "77in" `shouldBe` False

        it "must be a number followed by cm or in" $ do
            heightValid "160cm" `shouldBe` True
            heightValid "160" `shouldBe` False
            heightValid "abccm" `shouldBe` False
            heightValid "abcin" `shouldBe` False
            heightValid "abc" `shouldBe` False
