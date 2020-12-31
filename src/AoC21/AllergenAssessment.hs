{-# LANGUAGE TupleSections #-}
module AoC21.AllergenAssessment where

import Common.Utils
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Trifecta

aoc21 :: IO ()
aoc21 = do
  contents <- getInputFile 21
  let Success result = parseString parseIngredientSpec mempty contents
  print $ part1 result
  print "Done"

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