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

import ClassyPrelude hiding (last)
import Data.List (last, unfoldr)

iterate :: Int -> (Map Int Int, Int, Int) -> Int
iterate !limit (!state, !turn, !lastSpoken) =
  if turn == limit
  then lastSpoken
  else iterate limit (singletonMap lastSpoken turn <> state, turn + 1, turn - findWithDefault turn lastSpoken state)

initialize :: [Int] -> (Map Int Int, Int, Int)
initialize xs = (mapFromList $ xs `zip` [1..], length xs, last xs)

day15, day15alt, day15alt2 :: [Int]
day15 = [1,0,15,2,10,13]
day15alt = [0,3,6]
day15alt2 = [1,3,2]

main :: IO ()
main = do
  let numbers = day15
      limit = 30000000
  putStrLn . tshow . iterate limit . initialize $ numbers
