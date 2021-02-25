module AoC7.Haversacks where

import           Common.Utils
import           Data.List.Split hiding (sepBy)
import qualified Data.Map        as M
import qualified Data.Set        as S
import Text.Trifecta
import Control.Applicative ((<|>))

aoc7 :: IO ()
aoc7 = do
  contents <- getInputFile 7
  let p parser str = parseString parser mempty str
  --print $ p parseBag "wavy tan bags."
  --print $ p parseNumBags "2 dull crimson bags"
  --print $ p parseBagMap "2 mirrored green bags, 2 dull crimson bags, 2 drab tan bags, 1 vibrant coral bag."
  print $ p parseBagRule "dotted plum bags contain 2 mirrored green bags, 2 dull crimson bags, 2 drab tan bags, 1 vibrant coral bag."
  print $ p parseBagRule "drab tomato bags contain no other bags."
  print $ p parseBagRules contents

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

parseInput :: String -> BagRules
parseInput = M.fromList . map parseLine . lines

parseLine :: String -> (Bag, M.Map Bag Int)
parseLine str = (container, bags)
  where
    [containerStr, bagsStr] = splitOn " contain " str
    container = parseContainer containerStr
    bags = parseBags bagsStr

parseBags :: String -> M.Map Bag Int
parseBags str = case str of
  "no other bags." -> M.empty
  _                -> M.fromList $ map toBagTuple bagList
  where
    bagList = splitOn "," str
    toBagTuple s =
      let [number, hue, colour, _] = words s
       in (BG hue colour, read number)

parseContainer :: String -> Bag
parseContainer str = BG hue colour
  where
    [hue, colour, _] = words str

recursiveContainersForBag :: Bag -> BagRules -> S.Set Bag
recursiveContainersForBag = go S.empty
  where go found bg bgRules
            | null containers = found
            | otherwise = let allContainers = S.map (\x -> go containers x bgRules) containers in S.foldl S.union found allContainers
            where containers = containersForBag bg bgRules
                  containersForBag b = M.keysSet . M.filter (M.member b)


numberOfBagsForBag :: Bag -> BagRules -> Int
numberOfBagsForBag bag bagRules
  | null childBags = 0
  | otherwise = numberOfChildBags + grandChildren
  where childBags = M.findWithDefault M.empty bag bagRules
        numberOfChildBags = M.foldl (+) 0 childBags
        grandChildren = M.foldlWithKey (\count bag' number' -> count + number' * numberOfBagsForBag bag' bagRules) 0 childBags


--Another solution involve recursive knot tying

type Graph v e = M.Map v (M.Map v e)

allDescendants :: Ord v => Graph v e -> M.Map v (S.Set v)
allDescendants graph = descendantMap
  where
    descendantMap = M.foldMapWithKey (\v _ -> S.insert v (M.findWithDefault S.empty v descendantMap)) <$> graph

flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [ (m, M.singleton n e) | (n, ms) <- M.toList mp , (m, e) <- M.toList ms ]


allAncestors :: Ord v => Graph v e -> M.Map v (S.Set v)
allAncestors = allDescendants . flipGraph


usageCounts :: (Ord v, Num e) => Graph v e -> M.Map v e
usageCounts gr = usageMap
  where usageMap = (\mp -> sum [n * (M.findWithDefault 0 v usageMap + 1) | (v, n) <- M.toList mp]) <$> gr