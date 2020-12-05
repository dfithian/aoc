#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Control.Monad (fail)

allSeatScores :: Either Text [Int]
allSeatScores =
  let allSeats :: [[Char]] = do
        r1 <- ['F', 'B']
        r2 <- ['F', 'B']
        r3 <- ['F', 'B']
        r4 <- ['F', 'B']
        r5 <- ['F', 'B']
        r6 <- ['F', 'B']
        r7 <- ['F', 'B']
        c1 <- ['L', 'R']
        c2 <- ['L', 'R']
        c3 <- ['L', 'R']
        pure [r1, r2, r3, r4, r5, r6, r7, c1, c2, c3]
  in traverse seatScore allSeats

seatScore :: [Char] -> Either Text Int
seatScore xs = do
  let (row, col) = splitAt 7 xs
  r <- seatRow row
  c <- seatCol col
  pure $ 8 * r + c

seatRow :: [Char] -> Either Text Int
seatRow = map sum . traverse binRow . zip [6, 5..0]
  where
    binRow (p, c) = case c of
      'F' -> Right 0
      'B' -> Right $ 2 ^ p
      _ -> Left $ pack [c] <> ": not a valid row"

seatCol :: [Char] -> Either Text Int
seatCol = map sum . traverse binCol . zip [2, 1, 0]
  where
    binCol (p, c) = case c of
      'L' -> Right 0
      'R' -> Right $ 2 ^ p
      _ -> Left $ pack [c] <> ": not a valid col"

main :: IO ()
main = do
  seats <- lines . unpack <$> readFileUtf8 "day5.txt"
  allScores <- either (fail . unpack) (pure . asSet . setFromList) allSeatScores
  scores <- either (fail . unpack) (pure . setFromList) $ traverse seatScore seats
  let missing = setToList $ difference allScores scores
  putStrLn $ "Missing scores are " <> intercalate ", " (map tshow missing)
  -- putStrLn $ "Top score is " <> maybe "<empty>" tshow (headMay . reverse . sort $ scores)
