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
import qualified Data.Map as Map

realInput, altInput :: [Int]
realInput = [1, 3, 7, 8, 2, 6, 4, 9, 5]
altInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]

findDestination :: Int -> Map Int Int -> IO (Int, Int)
findDestination dest xs = case dest of
  neg | neg <= 0 -> case Map.lookupMax xs of
    Nothing -> fail "No max destination"
    Just tup -> pure tup
  _ -> case lookup dest xs of
    Nothing -> fail $ "No less than destination " <> show dest
    Just tup -> pure (dest, tup)

move :: Int -> Map Int Int -> IO (Map Int Int)
move firstCup otherCups = do
  ignored@(ignoredX, ignoredY, ignoredZ) :: (Int, Int, Int) <- maybe (fail "No elements left!") pure $ do
    x <- lookup firstCup otherCups
    y <- lookup x otherCups
    z <- lookup y otherCups
    pure (x, y, z)
  newFirstCupTarget <- maybe (fail "No first cup target") pure $ lookup ignoredZ otherCups
  let consideredCups = deleteMap ignoredZ . deleteMap ignoredY . deleteMap ignoredX $ otherCups
      dest = fromMaybe 0 . headMay . reverse . setToList $ difference (asSet $ setFromList [(firstCup - 1), (firstCup - 2), (firstCup - 3)]) (asSet $ setFromList [ignoredX, ignoredY, ignoredZ])
  (left, right) <- findDestination dest consideredCups
  pure $ mapFromList [(firstCup, newFirstCupTarget), (left, ignoredX), (ignoredX, ignoredY), (ignoredY, ignoredZ), (ignoredZ, right)] <> otherCups

play :: (Int, Int, Map Int Int) -> IO (Map Int Int)
play = \ case
  (0, _, x) -> pure x
  (n, x, xs) -> do
    when (n `mod` 100000 == 0) $ putStrLn $ tshow n <> " iterations remaining"
    ys <- move x xs
    y <- maybe (fail $ show x <> " not found") pure $ lookup x ys
    play (n - 1, y, ys)

ordered :: Int -> Map Int Int -> Text
ordered x xs = tshow x <> maybe "" (flip ordered (deleteMap x xs)) (lookup x xs)

stars :: Map Int Int -> IO Int
stars xs = maybe (fail "No elements for stars!") pure $ do
  x <- lookup 1 xs
  y <- lookup x xs
  pure $ x * y

render :: Int -> Map Int Int -> [Text]
render x xs = (tshow x):(maybe [] (flip render (deleteMap x xs)) (lookup x xs))

main :: IO ()
main = do
  let input = realInput <> [10..1000000]
      firstCup = unsafeHead input
      moves = 10000000
  putStrLn . tshow =<< stars =<< play (moves, unsafeHead input, mapFromList $ input `zip` (drop 1 input <> [firstCup]))
