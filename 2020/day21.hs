#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude
import Control.Monad (fail)
import qualified Data.Attoparsec.ByteString as Atto

newtype Allergen = Allergen { unAllergen :: Text }
  deriving (Eq, Ord, Show)

newtype Ingredient = Ingredient { unIngredient :: Text }
  deriving (Eq, Ord, Show)

alpha :: Atto.Parser Text
alpha = decodeUtf8 <$> Atto.takeWhile1 (\ w -> (w >= 65 && w <= 90) || (w >= 97 && w <= 122))

allergenParser :: Atto.Parser Allergen
allergenParser = Allergen <$> alpha

ingredientParser :: Atto.Parser Ingredient
ingredientParser = Ingredient <$> alpha

allergensParser :: Atto.Parser [Allergen]
allergensParser = (Atto.string " (contains ") *> (Atto.sepBy1 allergenParser (Atto.string ", ") <|> (singleton <$> allergenParser)) <* Atto.string ")"

ingredientsParser :: Atto.Parser [Ingredient]
ingredientsParser = Atto.sepBy1 ingredientParser (Atto.string " ")

lineParser :: Atto.Parser ([Ingredient], [Allergen])
lineParser = (,) <$> ingredientsParser <*> allergensParser

fileParser :: Atto.Parser [([Ingredient], [Allergen])]
fileParser = Atto.sepBy1 lineParser (Atto.string "\n")

-- the ingredient that corresponds to that allergen is the one that is in all the ingredient lists where that allergen appears
getIngredientsForAllergen :: Allergen -> [([Ingredient], [Allergen])] -> IO (Set Ingredient)
getIngredientsForAllergen allergen xss =
  case foldr (\ next acc -> if allergen `elem` snd next then (setFromList $ fst next):acc else acc) mempty xss of
    [] -> fail $ "No ingredients for " <> show allergen
    x:xs -> pure $ foldr intersect x xs

countInstancesOfNonAllergenIngredients :: Set Ingredient -> [([Ingredient], [Allergen])] -> Int
countInstancesOfNonAllergenIngredients allergenIngredients = length . filter (not . flip member allergenIngredients) . concatMap fst

getAllergenIngredients :: Map Ingredient Allergen -> [(Allergen, Set Ingredient)] -> IO (Map Ingredient Allergen)
getAllergenIngredients acc = \ case
  [] -> pure acc
  (allergen, xs):xss -> case headMay $ setToList xs of
    Just x -> case member x acc of
      True -> fail $ "Could not find a place for " <> show x <> " from " <> show allergen <> " in " <> show acc
      False -> getAllergenIngredients (insertMap x allergen acc) (sortOn (length . snd) $ map (\ (a, is) -> (a, deleteSet x is)) xss)
    Nothing -> fail $ "No ingredients left for " <> show allergen

main :: IO ()
main = do
  ingredientsWithAllergens <- either fail pure . Atto.parseOnly fileParser =<< readFile "day21.txt"
  let allAllergens = ordNub . concatMap snd $ ingredientsWithAllergens
  ingredientsByAllergen <- map (filter (not . null) . sortOn (length . snd)) . for allAllergens $ \ allergen -> do
    (allergen,) <$> getIngredientsForAllergen allergen ingredientsWithAllergens
  allergenIngredients <- getAllergenIngredients mempty ingredientsByAllergen
  putStrLn . intercalate "," . map (unIngredient . fst) . sortOn snd . mapToList $ allergenIngredients
