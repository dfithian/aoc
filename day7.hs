#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Data.Text (splitOn)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

newtype BagType = BagType { unBagType :: Text }
  deriving (Eq, Ord, Show)

data Bag = Bag
  { bagContents :: Map BagType Int
  } deriving (Eq, Show)

toBag :: Text -> Map BagType Bag
toBag x =
  let (rawTyp, rawContents) = break ((==) "bags") . words $ x
      typ = BagType . unwords $ rawTyp
      contents = case drop 2 rawContents of
        [] -> error "Expected contents"
        "no":_ -> mempty
        other -> foldr (\ next acc -> getOneContent acc $ words next) mempty . splitOn "," . unwords $ other
  in singletonMap typ $ Bag contents
  where
    getOneContent :: Map BagType Int -> [Text] -> Map BagType Int
    getOneContent acc = \ case
      [] -> error "Missing content"
      nRaw:rest ->
        let n = case readMay $ unpack nRaw of
              Nothing -> error $ "Failed to parse " <> unpack nRaw <> " as int"
              Just m -> m
        in insertMap (BagType . unwords . dropEnd 1 $ rest) n acc

canHasBag :: Map BagType Bag -> BagType -> BagType -> Bool
canHasBag bags contained container =
  case lookup container bags of
    Nothing -> error $ "Bag " <> show container <> " missing from all bags"
    Just (Bag contents) -> contained `elem` keys contents || any (canHasBag bags contained) (keys contents)

countContained :: Map BagType Bag -> BagType -> Int
countContained bags container =
  case lookup container bags of
    Nothing -> error $ "Bag " <> show container <> " missing from all bags"
    Just (Bag contents) -> sum $ flip map (mapToList contents) $ \ (typ, n) ->
      n + n * countContained bags typ

main :: IO ()
main = do
  bags <- foldr (\ next acc -> union (toBag next) acc) mempty . lines
    <$> readFileUtf8 "day7.txt"
  let insideShinyGold = countContained bags (BagType "shiny gold")
  putStrLn $ "Shiny gold contains " <> tshow insideShinyGold
