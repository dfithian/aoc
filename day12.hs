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

data Dir = N | S | E | W
  deriving (Eq, Show)
data Face = L | R | F
  deriving (Eq, Show)
data Instr
  = InstrDir Dir
  | InstrFace Face
  deriving (Eq, Show)

parseRow :: String -> IO (Instr, Int)
parseRow = \ case
  [] -> fail "Not a valid row"
  x:xs -> do
    i <- case x of
      'N' -> pure $ InstrDir N
      'S' -> pure $ InstrDir S
      'E' -> pure $ InstrDir E
      'W' -> pure $ InstrDir W
      'L' -> pure $ InstrFace L
      'R' -> pure $ InstrFace R
      'F' -> pure $ InstrFace F
      _ -> fail $ [x] <> " not a valid dir or face"
    n <- maybe (fail $ xs <> " not a valid int") pure $ readMay xs
    pure (i, n)

rotateLeft :: (Int, Int) -> Int -> (Int, Int)
rotateLeft (wayX, wayY) = \ case
  0 -> (wayX, wayY)
  90 -> (-wayY, wayX)
  180 -> (-wayX, -wayY)
  270 -> (wayY, -wayX)
  n -> error $ show n <> " invalid rotation"

rotateRight :: (Int, Int) -> Int -> (Int, Int)
rotateRight waypoint n = rotateLeft waypoint $ 360 - n

goInDirection :: Dir -> (Int, Int) -> Int -> (Int, Int)
goInDirection d (x, y) n =
  case d of
    N -> (x, y + n)
    S -> (x, y - n)
    E -> (x + n, y)
    W -> (x - n, y)

moveTowardsWaypoint :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
moveTowardsWaypoint (wayX, wayY) (shipX, shipY) n = (shipX + n * wayX, shipY + n * wayY)

doInstruction :: ((Int, Int), (Int, Int)) -> (Instr, Int) -> ((Int, Int), (Int, Int))
doInstruction (waypoint, ship) (instr, n) =
  case instr of
    InstrDir dir -> (goInDirection dir waypoint n, ship)
    InstrFace F -> (waypoint, moveTowardsWaypoint waypoint ship n)
    InstrFace L -> (rotateLeft waypoint n, ship)
    InstrFace R -> (rotateRight waypoint n, ship)

main :: IO ()
main = do
  instructions <- traverse parseRow . lines . unpack
    =<< readFileUtf8 "day12.txt"
  let (x, y) = snd $ foldl' doInstruction ((10, 1), (0, 0)) instructions
  putStrLn $ tshow $ abs x + abs y
