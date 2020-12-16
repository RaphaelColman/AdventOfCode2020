{-# LANGUAGE ScopedTypeVariables #-}
module AoC16.TicketTranslation where

import Common.Utils
import Data.List.Split
import Prelude hiding (min, max)
import qualified Data.Set as S
import Data.List
import qualified Data.IntMap as IM
import Debug.Trace

aoc16 :: IO ()
aoc16 = do
    contents <- getInputFile 16
    let (ticketRules, tickets, myTicket) = splitInput contents
    print $ part1 tickets ticketRules
    print $ part2 ticketRules tickets myTicket

part2 :: [TicketRule] -> [Ticket] -> Ticket -> Maybe Int
part2 ticketRules tickets myTicket = do
       let validTickets = stripInvalidTickets tickets ticketRules
       simplified <- runSimplify $ findTicketRuleMap validTickets ticketRules
       let dColumns = departureColumns simplified
       pure $ product $ map (myTicket !!) dColumns

departureColumns :: RuleMap -> [Int]
departureColumns = IM.keys . IM.filter (\s -> let name = S.findMin s in "departure" `isPrefixOf` name)
data TicketRule = TR {
    name :: String,
    rules :: [Range]
} deriving (Show, Eq, Ord)

data Range = Range {
    min :: Int,
    max :: Int
} deriving (Show, Eq, Ord)

type Ticket = [Int]

parseTicketRuleLine :: String -> TicketRule
parseTicketRuleLine str = TR name rules
    where [name, rest] = splitOn ":" str
          rules = map parseToRange $ filter ('-' `elem`) $ words rest
          parseToRange str = let [min, max] = splitOn  "-" str in Range (read min) (read max)

parseTicket :: String -> Ticket
parseTicket str = map read $ splitOn "," str

splitInput :: String -> ([TicketRule], [Ticket], [Int])
splitInput str = (rules, ranges, myTicket)
    where [part1, part2, part3] = splitWhen (=="") $ lines str
          rules = map parseTicketRuleLine part1
          ranges = map parseTicket $ filter (/= "nearby tickets:") part3
          myTicket :: [Int] = map read $ splitOn "," $ head $ filter (/="your ticket:") part2

valueInRange :: Int -> Range -> Bool
valueInRange i (Range min max) = i >=min && i <= max

valueFailsTicketRule :: Int -> TicketRule -> Bool
valueFailsTicketRule i (TR name ranges) = (not . any (valueInRange i)) ranges

valueFailsTicketRules :: Int -> [TicketRule] -> Bool
valueFailsTicketRules i = all (valueFailsTicketRule i)

invalidValues :: [Ticket] -> [TicketRule] -> [Int]
invalidValues tickets rules = filter (`valueFailsTicketRules` rules) $ concat tickets

part1 :: [Ticket] -> [TicketRule] -> Int
part1 tickets rules = sum $ invalidValues tickets rules

rulesForInt :: Int -> [TicketRule] -> S.Set String
rulesForInt int = S.fromList . map name . filter (satisfiesTicketRule int)
    where satisfiesTicketRule i (TR _ ranges) = any (valueInRange i) ranges

rulesForInts :: [Int] -> [TicketRule] -> S.Set String
rulesForInts ints rules = foldl1 S.intersection $ map (`rulesForInt` rules) ints

type RuleMap = IM.IntMap (S.Set String)

findTicketRuleMap :: [[Int]] -> [TicketRule] -> RuleMap
findTicketRuleMap tickets ticketRules = IM.fromList $ zip [0..] asList
    where columns = transpose tickets
          asList = map (`rulesForInts` ticketRules) columns

simplifyPossibleTicketRules :: RuleMap -> Maybe RuleMap
simplifyPossibleTicketRules ruleMap = do
    let singles = map (S.findMin . snd) $ IM.toList $ IM.filter (\s -> length s == 1) ruleMap
    if null singles
    then Nothing
    else Just $ foldl' (\rm str -> IM.map (deleteIfNotSingleton str) rm) ruleMap singles

deleteIfNotSingleton :: (Ord a ) => a -> S.Set a -> S.Set a
deleteIfNotSingleton elem set = if length set >= 2 then S.delete elem set else set

allSingletons :: RuleMap -> Bool
allSingletons = null . IM.filter (\ s -> length s /= 1)

runSimplify :: RuleMap -> Maybe RuleMap
runSimplify ruleMap = do
    simplified <- simplifyPossibleTicketRules ruleMap
    if allSingletons simplified
        then Just simplified
        else if simplified == ruleMap then Nothing
        else runSimplify simplified
    

stripInvalidTickets :: [Ticket] -> [TicketRule] -> [Ticket]
stripInvalidTickets tickets ticketRules = filter (\t -> not (failed t ticketRules)) tickets
    where failed ticket ticketRules = any (`valueFailsTicketRules` ticketRules) ticket