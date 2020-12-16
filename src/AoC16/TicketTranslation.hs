module AoC16.TicketTranslation where

import Common.Utils
import Data.List.Split
import Prelude hiding (min, max)

aoc16 :: IO ()
aoc16 = do
    contents <- getInputFile 16
    let (ticketRules, tickets) = splitInput contents
    print $ part1 tickets ticketRules

data TicketRule = TR {
    name :: String,
    rules :: [Range]
} deriving (Show, Eq)

data Range = Range {
    min :: Int,
    max :: Int
} deriving (Show, Eq)

type Ticket = [Int]

parseTicketRuleLine :: String -> TicketRule
parseTicketRuleLine str = TR name rules
    where [name, rest] = splitOn ":" str
          rules = map parseToRange $ filter ('-' `elem`) $ words rest
          parseToRange str = let [min, max] = splitOn  "-" str in Range (read min) (read max)

parseTicket :: String -> Ticket
parseTicket str = map read $ splitOn "," str

splitInput :: String -> ([TicketRule], [Ticket])
splitInput str = (rules, ranges)
    where [part1, part2, part3] = splitWhen (=="") $ lines str
          rules = map parseTicketRuleLine part1
          ranges = map parseTicket $ filter (/= "nearby tickets:") part3

satisfiesRange :: Ticket -> Range -> Bool
satisfiesRange ticket' (Range min max) = all (\t -> t >= min && t <= max) ticket'

satisfiesTicketRule :: Ticket -> TicketRule -> Bool
satisfiesTicketRule ticket' (TR name rules) = all (satisfiesRange ticket') rules

satisfiesTicketRules :: Ticket -> [TicketRule] -> Bool 
satisfiesTicketRules ticket' = all (satisfiesTicketRule ticket')

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