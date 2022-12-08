#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

import Data.List (foldl')
import "containers" Data.Map (Map, (!))
import qualified "containers" Data.Map.Strict as Map

data Height = Height Int
  deriving (Eq, Ord, Show)

type Grid = Map (Int, Int) Height

build :: String -> ((Int, Int), Grid, [(Int, Int)])
build heightss =
  let grid = foldr (\(row, heights) acc -> foldr (\(col, height) -> Map.insert (row, col) (Height (read [height]))) acc . zip [0..] $ heights) mempty . zip [0..] . lines $ heightss
      (maxRow, maxCol) = maximum . Map.keys $ grid
   in ((maxRow, maxCol), grid, [(row, col) | row <- [0..maxRow], col <- [0..maxCol]])

visibleOnSide :: Height -> Grid -> [(Int, Int)] -> Bool
visibleOnSide height grid = \case
  [] -> True
  points -> all (\point -> grid ! point < height) points

visible :: (Int, Int) -> Grid -> (Int, Int) -> Bool
visible (maxRow, maxCol) grid (row, col) =
  let left = [(row, y) | y <- [0..(col - 1)]]
      right = [(row, y) | y <- [(col + 1)..maxCol]]
      top = [(x, col) | x <- [0..(row - 1)]]
      bottom = [(x, col) | x <- [(row + 1)..maxRow]]
      height = grid ! (row, col)
   in any (visibleOnSide height grid) [left, right, top, bottom]

viewingDistance :: Height -> Grid -> [(Int, Int)] -> Int
viewingDistance height grid = fst . foldl' (\(n, b) point -> if b then (n, b) else let h = grid ! point in (n + 1, h >= height)) (0, False)

scenic :: (Int, Int) -> Grid -> (Int, Int) -> Int
scenic (maxRow, maxCol) grid (row, col) =
  let left = reverse [(row, y) | y <- [0..(col - 1)]]
      right = [(row, y) | y <- [(col + 1)..maxCol]]
      top = reverse [(x, col) | x <- [0..(row - 1)]]
      bottom = [(x, col) | x <- [(row + 1)..maxRow]]
      height = grid ! (row, col)
   in product . fmap (viewingDistance height grid) $ [left, right, top, bottom]

part1 :: IO ()
part1 = do
  (bounds, grid, points) <- build <$> readFile "day8.txt"
  putStrLn . show . length . filter (visible bounds grid) $ points

part2 :: IO ()
part2 = do
  (bounds, grid, points) <- build <$> readFile "day8.txt"
  putStrLn . show . maximum . fmap (scenic bounds grid) $ points

main :: IO ()
main = part2
