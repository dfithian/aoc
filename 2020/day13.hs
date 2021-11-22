#!/usr/bin/env stack
-- stack --resolver lts script

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
import Data.List ((!!))
import Data.Text (splitOn)

data BusId = BusId Int | NoBusId
  deriving (Eq, Show)

parseFile :: [String] -> IO (Int, [BusId])
parseFile = \ case
  xs@([n, ids]) -> case (,) <$> readMay n <*> traverse readBusId (splitOn "," $ pack ids) of
    Nothing -> fail $ show xs <> " not valid"
    Just x -> pure x
  other -> fail $ show other <> " not valid"
  where
    readBusId = \ case
      "x" -> Just NoBusId
      i -> BusId <$> readMay i

busIdMatches :: [BusId] -> Int -> Int -> [BusId]
busIdMatches busIds t0 tn = flip filter busIds $ \ case
  NoBusId -> False
  BusId busId -> (t0 + tn) `mod` busId == 0

main :: IO ()
main = do
  (time, busIds) <- parseFile . lines . unpack =<< readFileUtf8 "day13.txt"
  let relevantBusIds = flip concatMap (zip [0..] busIds) $ \ case
        (_, NoBusId) -> []
        (i, BusId n) -> [(i, n)]
  putStrLn $ "Relevants: " <> tshow relevantBusIds
  putStrLn $ "Product: " <> tshow (product $ snd <$> relevantBusIds)
  putStrLn $ tshow $ headMay $ concatMap (\ tn -> (tn,) <$> busIdMatches busIds time tn) [0..]
