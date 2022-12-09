#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

import Data.Function (on)
import Data.List (foldl', minimumBy)
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

data Direction = L | R | U | D
  deriving (Show)

data State = State (Int, Int) (Int, Int) (Set (Int, Int))
  deriving (Show)

data State10 = State10 (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Int, Int) (Set (Int, Int))

instance Show State10 where
  show (State10 a b c d e f g h i j visited) =
    "---\n" <> show [a, b, c, d, e, f, g, h, i, j] <> "\n" <> showVisited visited <> "\n---"

sVisited :: State -> Set (Int, Int)
sVisited (State _ _ visited) = visited

sVisited10 :: State10 -> Set (Int, Int)
sVisited10 (State10 _ _ _ _ _ _ _ _ _ _ visited) = visited

showVisited :: Set (Int, Int) -> String
showVisited s =
  let xs = fst <$> Set.toList s
      ys = snd <$> Set.toList s
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)
   in unlines . fmap (\(x, ys) -> fmap (\y -> if (x, y) == (0, 0) then 's' else if Set.member (x, y) s then '#' else '.') ys) . fmap (, [minY..maxY]) $ [minX..maxX]

parse :: [String] -> (Direction, Int)
parse = \case
  ["L", n] -> (L, read n)
  ["R", n] -> (R, read n)
  ["U", n] -> (U, read n)
  ["D", n] -> (D, read n)

sign :: Int -> Int -> (Int -> Int)
sign a b = \c -> c + if a < b then 1 else (-1)

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hx, hy) (tx, ty) = case abs (hx - tx) + abs (hy - ty) of
  big | big > 2 -> (sign tx hx tx, sign ty hy ty)
  2 -> case (hx == tx, hy == ty) of
    (True, False) -> (tx, sign ty hy ty)
    (False, True) -> (sign tx hx tx, ty)
    _ -> (tx, ty)
  _ -> (tx, ty)

move :: Direction -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
move d h t = (newH, follow newH t)
  where
    newH = case d of
      L -> (fst h - 1, snd h)
      R -> (fst h + 1, snd h)
      U -> (fst h, snd h + 1)
      D -> (fst h, snd h - 1)

state :: Direction -> State -> State
state direction (State h t visited) =
  let (newH, newT) = move direction h t
   in State newH newT (Set.insert newT visited)

state10 :: Direction -> State10 -> State10
state10 direction (State10 a b c d e f g h i j visited) =
  let (newA, newB) = move direction a b
      newC = follow newB c
      newD = follow newC d
      newE = follow newD e
      newF = follow newE f
      newG = follow newF g
      newH = follow newG h
      newI = follow newH i
      newJ = follow newI j
      newVisited = Set.insert newJ visited
   in State10 newA newB newC newD newE newF newG newH newI newJ newVisited

o :: (Int, Int)
o = (0, 0)

initial :: State
initial = State o o (Set.singleton o)

initial10 :: State10
initial10 = State10 o o o o o o o o o o (Set.singleton o)

part1 :: IO ()
part1 = do
  putStrLn . show . Set.size . sVisited . foldl' (\s (d, n) -> foldr (\next -> state next) s (replicate n d)) initial . fmap (parse . words) . lines
    =<< readFile "day9.txt"

part2 :: IO ()
part2 = do
  putStrLn . show . Set.size . sVisited10 . foldl' (\s (d, n) -> foldr state10 s (replicate n d)) initial10 . fmap (parse . words) . lines
    =<< readFile "day9.txt"

main :: IO ()
main = part2
