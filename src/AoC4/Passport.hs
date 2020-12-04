{-# LANGUAGE ScopedTypeVariables #-}

module AoC4.Passport where

import Data.Char
import Data.List.Split
import qualified Data.Set as S
import System.IO
import Text.Regex.TDFA

main :: IO ()
main = do
  handle <- openFile "src/AoC4/input.txt" ReadMode
  contents <- hGetContents handle
  let passports = map documentToPassport $ splitToDocuments contents
  print $ length passports
  print $ numberOfValidPassports passports

splitToDocuments :: String -> [String]
splitToDocuments = map (\c -> if c == "\n" then " " else c) . splitOn "\n\n"

requiredFields :: S.Set String
requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

documentToPassport :: String -> Passport
documentToPassport str = map parseField $ words str

parseField :: String -> PassportField
parseField str = (fieldName, value)
  where
    [fieldName, value] = splitOn ":" str

numberOfValidPassports :: [Passport] -> Int
numberOfValidPassports = length . filter passportIsValid

passportIsValid :: Passport -> Bool
passportIsValid psprt = hasAllRequiredFields psprt
  && all passportFieldIsValid psprt

hasAllRequiredFields :: Passport -> Bool
hasAllRequiredFields fields = S.isSubsetOf requiredFields presentFields
  where presentFields = S.fromList $ map fst fields

passportFieldIsValid :: PassportField -> Bool
passportFieldIsValid pf = case fieldName of
  "byr" -> birthyearValid value
  "iyr" -> issueYearValid value
  "eyr" -> expiryYearValid value
  "hgt" -> heightValid value
  "hcl" -> hairColourValid value
  "ecl" -> eyeColourValid value
  "pid" -> passportIdValid value
  "cid" -> True
  _ -> False
  where fieldName = fst pf
        value = snd pf

type PassportField = (String, String)
type Passport = [PassportField]


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
    hairColourPattern = "\\`#[0-9a-f]{6}\\'"

eyeColourValid :: String -> Bool
eyeColourValid str = str =~ eyeColourPattern :: Bool
  where
    eyeColourPattern = "amb|blu|brn|gry|grn|hzl|oth"

passportIdValid :: String -> Bool
passportIdValid str = str =~ passportIdPattern :: Bool
  where
    passportIdPattern = "\\`[0-9]{9}\\'" --` = beginning of line and ' == end of line
