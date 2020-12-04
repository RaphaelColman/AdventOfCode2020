{-# LANGUAGE ScopedTypeVariables #-}

module AoC4.Passport where

import Data.Char
import Data.List.Split
import qualified Data.Set as S
import System.IO
import Text.Regex.TDFA

mn :: IO ()
mn = do
  handle <- openFile "src/AoC4/input.txt" ReadMode
  contents <- hGetContents handle
  let mappedToFields = map fields $ splitToDocuments contents
  print $ length $ filter validPassportFields mappedToFields
  let passports = map documentToPassport $ splitToDocuments contents
  print "hello"

splitToDocuments :: String -> [String]
splitToDocuments = map (\c -> if c == "\n" then " " else c) . splitOn "\n\n"

fields :: String -> S.Set String
fields xs = S.fromList $ map getField $ words xs
  where
    getField = head . splitOn ":"

validPassportFields :: S.Set String -> Bool
validPassportFields = S.isSubsetOf requiredFields

requiredFields :: S.Set String
requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

documentToPassport :: String -> Passport
documentToPassport str = map parseField $ words str

parseField :: String -> PassportField
parseField str = case fieldName of
  "byr" -> BYR value
  "iyr" -> IYR value
  "eyr" -> EYR value
  "hgt" -> HGT value
  "hcl" -> HCL value
  "ecl" -> ECL value
  "pid" -> PID value
  "cid" -> CID value
  where
    [fieldName, value] = splitOn ":" str

passportIsValid :: Passport -> Bool
passportIsValid = all valid

hasAllRequiredFields :: [PassportField] -> Bool
hasAllRequiredFields fields = S.fromList fields

data PassportField = BYR String | IYR String | EYR String | HGT String | HCL String | ECL String | PID String | CID String deriving (Show)

type Passport = [PassportField]

class CanBeValid a where
  valid :: a -> Bool

instance CanBeValid PassportField where
  valid (BYR s) = birthyearValid s
  valid (IYR s) = issueYearValid s
  valid (EYR s) = expiryYearValid s
  valid (HGT s) = heightValid s
  valid (HCL s) = hairColourValid s
  valid (ECL s) = eyeColourValid s
  valid (PID s) = passportIdValid s
  valid (CID _) = True

birthyearValid :: String -> Bool
birthyearValid s =
  length s == 4
    && sAsInt >= 1920
    && sAsInt <= 2002
  where
    sAsInt :: Int = read s

issueYearValid :: String -> Bool
issueYearValid s =
  length s == 4
    && sAsInt >= 2010
    && sAsInt <= 2020
  where
    sAsInt :: Int = read s

expiryYearValid :: String -> Bool
expiryYearValid s =
  length s == 4
    && sAsInt >= 2020
    && sAsInt <= 2030
  where
    sAsInt :: Int = read s

heightValid :: String -> Bool
heightValid str
  | units == "cm" = valueAsInt >= 150 && valueAsInt <= 193
  | units == "in" = valueAsInt >= 59 && valueAsInt <= 76
  | otherwise = False
  where
    (valueString, units) = span isDigit str --Span is AMAZING for this
    valueAsInt :: Int = read valueString

hairColourValid :: String -> Bool
hairColourValid str = str =~ hairColourPattern :: Bool
  where
    hairColourPattern = "#[1-9a-f]{6}"

eyeColourValid :: String -> Bool
eyeColourValid str = str =~ eyeColourPattern :: Bool
  where
    eyeColourPattern = "amb|blu|brn|gry|grn|hzl|oth"

passportIdValid :: String -> Bool
passportIdValid str = str =~ passportIdPattern :: Bool
  where
    passportIdPattern = "\\`[0-9]{9}\\'" --` = beginning of line and ' == end of line
