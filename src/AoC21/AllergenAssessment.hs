{-# LANGUAGE TupleSections #-}
module AoC21.AllergenAssessment where

import Common.Utils
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Trifecta
import Data.List

aoc21 :: IO ()
aoc21 = do
  contents <- getInputFile 21
  let parsed = parseString parseIngredientSpec mempty contents
  print $ part1 <$> parsed
  print $ part2 <$> parsed

type Ingredient = String

type IngredientList = Set Ingredient

type Allergen = String

type AllergenList = Set Allergen

type IngredientEntry = (IngredientList, AllergenList)
type IngredientSpec = [IngredientEntry]
type IngredientMap = Map Ingredient AllergenList
type Suggestion = (Ingredient, Allergen)

part1 :: IngredientSpec -> Int
part1 ingredientSpec = foldr (\ingredient currentCount -> currentCount + (counts M.! ingredient)) 0 safe
    where safe = safeIngredients ingredientSpec
          allListedIngredients = foldr (\(il,al) xs -> mappend xs (S.toList il)) [] ingredientSpec
          counts = freqs allListedIngredients

part2 :: IngredientSpec -> String
part2 ingredientSpec = intercalate "," sorted
  where simplified = M.toList $ simplifyMap $ simplify ingredientSpec
        sorted = map fst $ sortOn snd simplified

safeIngredients :: IngredientSpec -> Set Ingredient
safeIngredients = M.keysSet . M.filter null . simplify

initIngredientMap :: IngredientSpec -> IngredientMap
initIngredientMap = foldr rollUp M.empty
    where rollUp (il, al) mp = S.foldl (\mp' i -> M.insertWith mappend i al mp') mp il

suggestions :: IngredientMap -> Set Suggestion
suggestions = M.foldMapWithKey rollUp
    where rollUp ingredient = S.map (ingredient,)

trySuggestion :: IngredientSpec -> Suggestion -> Bool
trySuggestion spec (ingredient, allergen) = specValid mapped
    where mapped = map tryOnEntry spec
          tryOnEntry originalEntry@(il, al) = if allergen `S.member` al
                                then let filtered = S.filter (==ingredient) il in (filtered, al)
                                else originalEntry
          specValid = not . any (\(il, al) -> null il) 


simplify :: IngredientSpec -> IngredientMap
simplify ingredientSpec = S.foldl removeIfImpossible ingredientMap allSuggestions
    where ingredientMap = initIngredientMap ingredientSpec
          allSuggestions = suggestions ingredientMap
          removeIfImpossible mp suggestion'@(ingredient, allergen) = if trySuggestion ingredientSpec suggestion'
                                             then mp
                                             else M.update (Just . S.delete allergen) ingredient mp

simplifyMapStep :: IngredientMap -> IngredientMap
simplifyMapStep ingredientMap = M.foldlWithKey updateForSingle ingredientMap singles
    where singles = M.map S.findMin $ M.filter (\a -> length a == 1) ingredientMap
          updateForSingle mp ingredient allergen = M.mapWithKey (\ingredient' allergens -> 
                                                      if ingredient' == ingredient
                                                      then allergens
                                                      else S.delete allergen allergens
                                                      ) mp

simplifyMap :: IngredientMap -> Map Ingredient Allergen
simplifyMap = M.map S.findMin . M.filter (\x -> length x == 1) . last . unfoldr doSimplify
  where doSimplify :: IngredientMap -> Maybe (IngredientMap, IngredientMap)
        doSimplify ingredientMap
          | all (\s -> length s <= 1) ingredientMap = Nothing
          | otherwise = let simplified = simplifyMapStep ingredientMap in Just (simplified, simplified)

---Parsing---
parseIngredientList :: Parser IngredientList
parseIngredientList = do
    list <- some $ token $ some letter
    pure $ S.fromList list

parseAllergenList :: Parser AllergenList
parseAllergenList = do
  list <- parens
        ( do
            string "contains"
            whiteSpace
            commaSep $ some letter
        )
  pure $ S.fromList list

parseIngredientEntry :: Parser (IngredientList, AllergenList)
parseIngredientEntry = do
  ingredients <- parseIngredientList
  allergens <- parseAllergenList
  pure (ingredients, allergens)

parseIngredientSpec :: Parser IngredientSpec
parseIngredientSpec = token (some parseIngredientEntry)