module AoC4.Passport where

import System.IO
import Data.List.Split
import qualified Data.Set as S

mn :: IO ()
mn = do
  handle <- openFile "src/AoC4/input.txt" ReadMode
  contents <- hGetContents handle
  let mappedToFields = map fields $ splitToDocuments contents
  print $ length $ filter validPassportFields mappedToFields

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


data PassportField = BYR String | IYR String | EYR String | HGT String | ECL String | PID String deriving (Show)