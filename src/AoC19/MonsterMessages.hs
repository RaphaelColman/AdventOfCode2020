{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AoC19.MonsterMessages where

import           Common.Utils
import           Control.Monad
import           Data.Either
import           Data.Foldable
import           Data.Functor    ((<&>))
import qualified Data.IntMap     as IM
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           GHC.Generics    (Generic)

aoc19 :: IO ()
aoc19 = do
    contents <- getInputFile 19
    let [rulesStr, messagesStr] = splitOn "\n\n" contents
    let ruleMap = parseRuleMap' rulesStr
    let messages = lines messagesStr
    print $ solver ruleMap messages
    print $ IM.lookup 8 (transform ruleMap)
    print $ IM.lookup 11 (transform ruleMap)
    print $ solver (transform ruleMap) messages

part1 :: IM.IntMap Rule -> [String] -> Maybe Int
part1 = solver

--transform :: RuleMap -> RuleMap
transform :: IM.IntMap Rule -> IM.IntMap Rule
transform = IM.mapWithKey theUpdate
    where theUpdate key val
            | key == 8 = Compound $ Or [And [Leaf 42], And [Leaf 42, Leaf 8]]
            | key == 11 = Compound $ Or [And [Leaf 42, Leaf 31], And [Leaf 42, Leaf 11, Leaf 31]]
            | otherwise = val

data AndOr a = Leaf a
             | And [AndOr a]
             | Or  [AndOr a]
  deriving (Show, Eq, Ord, Generic, Functor)

instance Applicative AndOr where
    pure  = return
    (<*>) = ap

instance Monad AndOr where
    return  = Leaf
    ao >>= f = case ao of
      Leaf x -> f x
      And xs -> And $ map (>>= f) xs
      Or  xs -> Or  $ map (>>= f) xs

data Rule = Simple Char
          | Compound (AndOr Int)
  deriving (Show, Eq, Ord, Generic)

expandRules :: IM.IntMap Rule -> IM.IntMap (AndOr Char)
expandRules rules = res
  where
    res = rules <&> \case
      Simple c    -> Leaf c
      Compound cs -> cs >>= (res IM.!)

match :: AndOr Char -> String -> [String]
match = \case
    Leaf c -> \case
      []     -> []
      q : qs -> qs <$ guard (q == c)
    And xs -> foldr (>=>) pure (match <$> xs)
    Or  xs -> \str -> concatMap (`match` str) xs

solver :: IM.IntMap Rule -> [String] -> Maybe Int
solver rs ss = do
    rule <- IM.lookup 0 (expandRules rs)
    pure $ countTrue (any null . match rule) ss

countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

parseRuleMap' :: String -> IM.IntMap Rule
parseRuleMap' = IM.fromList . map parseRuleLine' . lines

parseRuleLine' :: String -> (Int, Rule)
parseRuleLine' str = (read num, value)
    where [num, rest] = splitOn ":" str
          value = if '\"' `elem` rest
                  then Simple (rest !! 2)
                  else parseCompositeRule' rest

parseCompositeRule' :: String -> Rule
parseCompositeRule' str = Compound rule
    where compositeRule :: [[Int]] = map (map read . words) $ splitOn "|" str
          rule = Or $ map (And . map Leaf) compositeRule
