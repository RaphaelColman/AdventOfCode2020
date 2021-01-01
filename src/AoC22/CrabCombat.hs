module AoC22.CrabCombat where

import Common.Utils
import Data.Foldable
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Text.Trifecta
import Data.Maybe
import Debug.Trace

aoc22 :: IO ()
aoc22 = do
  contents <- getInputFile 22
  let result = parseString parseGame mempty contents
  print $ part1 <$> result
  print $ part2 <$> result
  print "done"

type Cards = Seq Integer

data Game = MkGame
  { player1 :: Cards,
    player2 :: Cards
  }
  deriving (Eq, Show, Ord)

data RecursiveGameParams = MkParams {
    currentGame :: Game,
    previousGames :: Set Game
} deriving (Eq, Show, Ord)
data Winner = Player1 | Player2 deriving (Eq, Show, Ord, Enum)
type RecursiveGameResult = (RecursiveGameParams, Winner)

initRecursiveGameParams :: Game -> RecursiveGameParams
initRecursiveGameParams game = MkParams game S.empty

playRecursiveGame :: RecursiveGameParams -> RecursiveGameResult
playRecursiveGame params@(MkParams currentGame@(MkGame player1 player2) previousGames)
    | currentGame `S.member` previousGames = (params, Player1)
    | finished currentGame = let wnr = fromJust (winner currentGame) in (params, wnr) --is fromJust appropriate?
    | otherwise = if not player1HasEnough || not player2HasEnough
                  then playFiniteRound 
                  else let copy = MkGame (Seq.take (fromInteger player1Card) player1Rest) (Seq.take (fromInteger player2Card) player2Rest)
                           newGame = MkParams copy S.empty
                           (_, wnr) = playRecursiveGame newGame
                       in if wnr == Player1
                          then playRecursiveGame player1WinsParams
                          else playRecursiveGame player2WinsParams
    where player1Card Seq.:< player1Rest = Seq.viewl player1
          player2Card Seq.:< player2Rest = Seq.viewl player2
          player1HasEnough = length player1Rest >= fromInteger player1Card
          player2HasEnough = length player2Rest >= fromInteger player2Card --Refactor into helper?
          playFiniteRound = let next = stepGame currentGame 
                                initNextParams = MkParams next (S.insert currentGame previousGames)
                            in playRecursiveGame initNextParams
          player1WinsDeck = player1Rest Seq.>< Seq.fromList [player1Card, player2Card]
          player2WinsDeck = player2Rest Seq.>< Seq.fromList [player2Card, player1Card]
          player1WinsParams = let gm = MkGame player1WinsDeck player2Rest in MkParams gm (S.insert currentGame previousGames)
          player2WinsParams = let gm = MkGame player1Rest player2WinsDeck in MkParams gm (S.insert currentGame previousGames)

part1 :: Game -> Maybe Integer
part1 game = do
  winner <- toList . Seq.reverse <$> winningHand (playGame game)
  pure $ sum $ zipWith (*) winner [1 ..]

part2 :: Game -> Maybe Integer
part2 game = do
  let (MkParams game' _, _) = playRecursiveGame $ initRecursiveGameParams game
  winner <- toList . Seq.reverse <$> winningHand game'
  pure $ sum $ zipWith (*) winner [1 ..]

finished :: Game -> Bool --Maybe this should return a Maybe Winner (where nothing means the game is going on)
finished (MkGame player1 player2) = any null [player1, player2]

winner :: Game -> Maybe Winner
winner (MkGame player1Cards player2Cards)
  | null player2Cards = Just Player1
  | null player1Cards = Just Player2
  | otherwise = Nothing

winningHand :: Game -> Maybe Cards
winningHand (MkGame player1Cards player2Cards)
  | null player1Cards = Just player2Cards
  | null player2Cards = Just player1Cards
  | otherwise = Nothing

playGame :: Game -> Game
playGame game = last $ unfoldr playUntilWon game
  where
    playUntilWon gm
      | finished gm = Nothing
      | otherwise = let next = stepGame gm in Just (next, next)

stepGame :: Game -> Game
stepGame (MkGame player1 player2) =
  if player1Card > player2Card
    then MkGame player1Wins player2Rest
    else MkGame player1Rest player2Wins
  where
    player1Card Seq.:< player1Rest = Seq.viewl player1
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