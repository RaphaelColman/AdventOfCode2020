module AoC7.Haversacks where

import           Common.Utils
import           Data.List.Split hiding (sepBy)
import qualified Data.Map        as M
import qualified Data.Set        as S
import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Monoid

aoc7 :: IO ()
aoc7 = do
  contents <- getInputFile 7
  let bagRules = parseString parseBagRules mempty contents
  let bagToSearch = BG "shiny" "gold"
  print $ numberOfBagsForBag bagToSearch <$> bagRules
  let allSolutions = betterNumberOfBagsForBag <$> bagRules
  print $ M.lookup bagToSearch <$> allSolutions
  let graphySolution = graphyBags <$> bagRules
  print $ M.lookup bagToSearch <$> graphySolution
  print "done"

type BagRules = M.Map Bag (M.Map Bag Int)

data Bag = BG
  {
    hue    :: String,
    colour :: String
  }
  deriving (Show, Eq, Ord)

parseBag :: Parser Bag
parseBag = do
  hue <- some letter
  whiteSpace 
  colour <- some letter
  whiteSpace 
  string "bag"
  skipOptional $ char 's'
  skipOptional $ char '.'
  pure $ BG hue colour

parseNumBags :: Parser (Bag, Int)
parseNumBags = do
  num <- integer
  bag <- parseBag
  pure (bag, fromIntegral num)

parseBagMap :: Parser (M.Map Bag Int)
parseBagMap = do
  let noOtherBags = fmap (const []) $ try $ string "no other bags"
  let bags = sepBy parseNumBags $ string ", " 
  bagsResult <- noOtherBags <|> bags
  pure $ M.fromList bagsResult

parseBagRule :: Parser (Bag, M.Map Bag Int)
parseBagRule = do
  containingBag <- parseBag
  whiteSpace >> string "contain" >> whiteSpace
  bagMap <- parseBagMap
  pure (containingBag, bagMap)

parseBagRules :: Parser BagRules
parseBagRules = do
  bagRules <- some (token parseBagRule)
  pure $ M.fromList bagRules

recursiveContainersForBag :: Bag -> BagRules -> S.Set Bag
recursiveContainersForBag = go S.empty
  where go found bg bgRules
            | null containers = found
            | otherwise = let allContainers = S.map (\x -> go containers x bgRules) containers in S.foldl S.union found allContainers
            where containers = containersForBag bg bgRules
                  containersForBag b = M.keysSet . M.filter (M.member b)


numberOfBagsForBag :: Bag -> BagRules -> Int
numberOfBagsForBag bag bagRules
  | null children = 0
  | otherwise = getSum (M.foldMapWithKey combine children)
  where children = M.findWithDefault M.empty bag bagRules
        combine bag' count = Sum (count + count * numberOfBagsForBag bag' bagRules)

betterNumberOfBagsForBag :: BagRules -> M.Map Bag Int
betterNumberOfBagsForBag bagRules = memo
  where memo = M.mapWithKey countBags bagRules
        countBags _ containedBags = getSum $ M.foldMapWithKey combine containedBags
        combine bag' count' = Sum (count' + count' * M.findWithDefault 0 bag' memo)


--Another solution involve recursive knot tying

type Graph n e = M.Map n (M.Map n e)

foldGraph :: (Monoid m, Ord n) => m -> (e -> m -> m) -> Graph n e -> M.Map n m
foldGraph def f graph = memo
  where memo = M.mapWithKey (\_ a -> M.foldMapWithKey combine a) graph
        combine node' edge' = let lookup = M.findWithDefault def node' memo
                                in f edge' lookup

graphyBags :: BagRules -> M.Map Bag (Sum Int)
graphyBags = foldGraph (Sum 0) combiningFun
  where combiningFun edge mon = Sum $ edge + edge * getSum mon