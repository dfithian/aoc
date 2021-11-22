#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Control.Monad (fail)

f :: [Int] -> Int
f xs = case length xs of
  0 -> 1
  1 -> 1
  2 -> 1
  n -> sum [1..(n - 2)] + 1

main :: IO ()
main = do
  joltages :: [Int] <- map sort . traverse (\ x -> maybe (fail $ x <> ": not a number") pure $ readMay x) . lines . unpack
    =<< readFileUtf8 "day10.txt"
  let (_, lastGroup, firstGroups) = foldl' (\ (prev, this, acc) next -> if next - prev == 1 then (next, next:this, acc) else (next, [next], (reverse this):acc)) (0, [], []) (0:joltages)
      groups = reverse $ lastGroup:firstGroups
      possibilities = f <$> groups
      allPossibilities = product possibilities
  putStrLn $ tshow groups
  putStrLn $ tshow possibilities
  putStrLn $ tshow allPossibilities
