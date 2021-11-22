#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude hiding (maximum, minimum)
import Control.Monad (fail)
import Data.List ((!!), unfoldr)
import Prelude (maximum, minimum)

isSumInWindow :: [Int] -> Int -> Bool
isSumInWindow ws n = any ((==) n) $ mconcat [if x == y then [] else [x + y] | x <- ws, y <- ws]

isContiguousSum :: Int -> [Int] -> Bool
isContiguousSum n xs = n == sum xs

slices :: [Int] -> [[Int]]
slices xs =
  mconcat $ map (\ i -> unfoldr f (i, xs)) [2..(length xs)]
  where
    f (sliceLength, toSlice) = case take sliceLength toSlice of
      invalid | length invalid < sliceLength -> Nothing
      slice -> Just (slice, (sliceLength, drop 1 toSlice))

main :: IO ()
main = do
  numbers :: [Int] <- traverse (\ x -> maybe (fail $ x <> ": not a number") pure $ readMay x) . lines . unpack
    =<< readFileUtf8 "day9.txt"
  let w = 25
  invalidIndex <- maybe (fail "No invalid numbers") pure . headMay $
    filter (\ i -> not $ isSumInWindow (take w $ drop (i - w) numbers) (numbers !! i)) [(w + 1)..(length numbers)]
  contiguousSum <- maybe (fail "No slices") pure . headMay . filter (isContiguousSum (numbers !! invalidIndex)) . slices $ numbers
  putStrLn . tshow $
    (minimum contiguousSum) + (maximum contiguousSum)
