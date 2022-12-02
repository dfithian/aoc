#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}

import Data.List (sum)

data Shape = Rock | Paper | Scissors deriving (Eq)

data Outcome = Lose | Draw | Win deriving (Eq)

mkShape :: Char -> Shape
mkShape = \case
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors

mkOutcome :: Char -> Outcome
mkOutcome = \case
  'X' -> Lose
  'Y' -> Draw
  'Z' -> Win

scoreShape :: Shape -> Int
scoreShape = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

scoreOutcome :: Outcome -> Int
scoreOutcome = \case
  Lose -> 0
  Draw -> 3
  Win -> 6

next :: Shape -> Shape
next = \case
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

prev :: Shape -> Shape
prev = next . next

scorePart1 :: Shape -> Shape -> Int
scorePart1 op me = scoreOutcome outcome + scoreShape me
  where
    outcome =
      if op == me then Draw
      else if op == next me then Lose
      else Win

scorePart2 :: Shape -> Outcome -> Int
scorePart2 shape outcome = scoreOutcome outcome + scoreShape need
  where
    need = case outcome of
      Lose -> prev shape
      Draw -> shape
      Win -> next shape

part1 :: IO ()
part1 =
  putStrLn . show . sum . fmap (\[op, _, me] -> scorePart1 (mkShape op) (mkShape me)) . lines 
    =<< readFile "day2.txt"

part2 :: IO ()
part2 =
  putStrLn . show . sum . fmap (\[p, _, o] -> scorePart2 (mkShape p) (mkOutcome o)) . lines 
    =<< readFile "day2.txt"

main :: IO ()
main = part2
