module AoC22.CrabCombat where

import Text.Trifecta
import Common.Utils
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List
import Data.Foldable

aoc22 :: IO ()
aoc22 = do
  contents <- getInputFile 22
  let result = parseString parseGame mempty contents
  print $ part1 <$> result
  print "done"

type Cards = Seq Integer

data Game = MkGame {
    player1 :: Cards,
    player2 :: Cards
} deriving (Eq, Show, Ord)

part1 :: Game -> Maybe Integer
part1 game = do
    winner <- toList . Seq.reverse <$> winningHand (playGame game)
    pure $ sum $ zipWith (*) winner [1..]

finished :: Game -> Bool
finished (MkGame player1 player2) = any null [player1, player2]

winningHand :: Game -> Maybe Cards
winningHand (MkGame player1Cards player2Cards)
    | null player1Cards = Just player2Cards
    | null player2Cards = Just player1Cards
    | otherwise = Nothing

playGame :: Game -> Game
playGame game = last $ unfoldr playUntilWon game
    where playUntilWon gm
            | finished gm = Nothing
            | otherwise = let next = stepGame gm in Just (next, next)
    
stepGame :: Game -> Game
stepGame (MkGame player1 player2) = if player1Card > player2Card
                                     then MkGame player1Wins player2Rest
                                     else MkGame player1Rest player2Wins
    where player1Card Seq.:< player1Rest = Seq.viewl player1
          player2Card Seq.:< player2Rest = Seq.viewl player2
          player1Wins = player1Rest Seq.>< Seq.fromList [player1Card, player2Card]
          player2Wins = player2Rest Seq.>< Seq.fromList [player2Card, player1Card]

--Parsing
parsePlayerCards :: Integer -> Parser Cards
parsePlayerCards num = do
    string "Player" >> whiteSpace
    string $ show num
    char ':' >> newline
    parseCards

parseCards :: Parser Cards
parseCards = do
    cards <- some $ token integer
    return $ Seq.fromList cards

parseGame :: Parser Game
parseGame = do
    player1 <- parsePlayerCards 1
    player2 <- parsePlayerCards 2
    return $ MkGame player1 player2