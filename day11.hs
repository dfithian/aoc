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

data Pos = Seat | Floor
  deriving (Eq, Show)
data Occ = Empty | Occ
  deriving (Eq, Show)
data Dir = L | R | S
  deriving (Eq, Show)
type Coord = (Int, Int)

printMap :: [[(Pos, Occ)]] -> IO ()
printMap pos = putStrLn . pack . unlines . flip map pos $ map $ \ case
  (Seat, Empty) -> 'L'
  (Seat, Occ) -> '#'
  (Floor, _) -> '.'

parseRow :: String -> IO [Pos]
parseRow = traverse $ \ case
  '.' -> pure Floor
  'L' -> pure Seat
  c -> fail $ [c] <> " is not a seat or floor"

nextCoordDir :: (Coord, (Dir, Dir)) -> Coord
nextCoordDir ((i, j), (dx, dy)) =
  let newI = case dx of
        L -> i - 1
        R -> i + 1
        S -> i
      newJ = case dy of
        L -> j - 1
        R -> j + 1
        S -> j
  in (newI, newJ)

safeGetAdjacent :: [[(Pos, Occ)]] -> (Int, Int) -> (Coord, (Dir, Dir)) -> Maybe (Pos, Occ)
safeGetAdjacent xs (maxRow, maxCol) ((i, j), (dx, dy)) = case (i >= 0 && i < maxRow && j >= 0 && j < maxCol) of
  True -> case (xs !! i) !! j of
    p@(Seat, _) -> Just p
    _ -> safeGetAdjacent xs (maxRow, maxCol) (nextCoordDir ((i, j), (dx, dy)), (dx, dy))
  False -> Nothing

safeGet :: [[(Pos, Occ)]] -> (Int, Int) -> Coord -> Maybe (Pos, Occ)
safeGet xs (maxRow, maxCol) (i, j) = case (i >= 0 && i < maxRow && j >= 0 && j < maxCol) of
  True -> Just $ (xs !! i) !! j
  False -> Nothing

adjacentCoords :: Coord -> [(Coord, (Dir, Dir))]
adjacentCoords (i, j) =
  [ ((i - 1, j - 1), (L, L))
  , ((i - 1, j), (L, S))
  , ((i - 1, j + 1), (L, R))
  , ((i, j - 1), (S, L))
  , ((i, j + 1), (S, R))
  , ((i + 1, j - 1), (R, L))
  , ((i + 1, j), (R, S))
  , ((i + 1, j + 1), (R, R))
  ]

coordShouldOcc :: [[(Pos, Occ)]] -> (Int, Int) -> Coord -> (Pos, Occ)
coordShouldOcc pos dims coord =
  let x = fromMaybe (error $ "Invalid coord " <> show coord) $ safeGet pos dims coord
      adjacents = map snd . catMaybes . map (safeGetAdjacent pos dims) . adjacentCoords $ coord
      adjacentsAreOcc = (>= 5) . length . filter ((==) Occ) $ adjacents
      adjacentsAreEmpty = all ((==) Empty) adjacents
  in case x of
    (Floor, _) -> x
    (Seat, Empty) -> (Seat, if adjacentsAreEmpty then Occ else Empty)
    (Seat, Occ) -> (Seat, if adjacentsAreOcc then Empty else Occ)

applyRules :: [[(Pos, Occ)]] -> (Int, Int) -> [[Coord]] -> IO (Int, [[(Pos, Occ)]])
applyRules startPos dims css = do
  let inner n pos = do
        let newPos = flip map css $ \ cs ->
              flip map cs $ \ c ->
                coordShouldOcc pos dims c
        putStrLn $ "Iteration " <> tshow n
        printMap newPos
        if pos == newPos then pure (n, pos) else inner (n + 1) newPos
  putStrLn "Starting"
  printMap startPos
  inner 1 startPos

main :: IO ()
main = do
  startPos :: [[(Pos, Occ)]] <- map (map (map (, Empty))) . traverse parseRow . lines . unpack
    =<< readFileUtf8 "day11.txt"
  let maxRow = length startPos
      maxCol = fromMaybe (error $ "No columns in " <> show startPos) . headMay . map length $ startPos
      coords = flip map [0..(maxRow - 1)] $ \ i -> (i,) <$> [0..(maxCol - 1)]
  (iterations, endPos) <- applyRules startPos (maxRow, maxCol) coords
  putStrLn $ tshow iterations
  putStrLn $ tshow . length . filter ((==) Occ) . map snd . mconcat $ endPos

