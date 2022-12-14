#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

import Data.List (foldl')
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

build :: String -> Set (Int, Int)
build = foldr f mempty . lines
  where
    readPoint x =
      let (a, b) = span ((/=) ',') x
       in (read a, read (drop 1 b))
    segment (x, y) (x', y') = case (compare x x', compare y y') of
      (LT, EQ) -> (, y) <$> [x..x']
      (GT, EQ) -> (, y) <$> [x'..x]
      (EQ, LT) -> (x, ) <$> [y..y']
      (EQ, GT) -> (x, ) <$> [y'..y]
    g (acc, pt) = \case
      "->" -> (acc, pt)
      next ->
        let pt' = readPoint next
         in (foldr Set.insert acc (segment pt pt'), pt')
    f nextLine acc = case words nextLine of
      [] -> acc
      x:xs -> fst $ foldl' g (acc, readPoint x) xs

origin :: (Int, Int)
origin = (500, 0)

rest :: ((Int, Int) -> Bool) -> Set (Int, Int) -> (Int, Int) -> (Int, Int)
rest stop points (x, y) = case stop (x, y) of
  True -> (x, y)
  False -> case (Set.member (x, y + 1) points, Set.member (x - 1, y + 1) points, Set.member (x + 1, y + 1) points) of
    (True, True, True) -> (x, y)
    (False, _, _) -> rest stop points (x, y + 1)
    (_, False, _) -> rest stop points (x - 1, y + 1)
    (_, _, False) -> rest stop points (x + 1, y + 1)

simulate :: Int -> Int -> Set (Int, Int) -> Int
simulate orig maxY points =
  let (x, y) = rest (\(_, y) -> y > maxY) points origin
   in if y > maxY then Set.size points - orig else simulate orig (max maxY y) (Set.insert (x, y) points)

simulate2 :: Int -> Int -> Set (Int, Int) -> Int
simulate2 orig maxY points =
  let new = rest (\(_, y) -> y == maxY - 1) points origin
   in if new == origin then Set.size points - orig + 1 else simulate2 orig maxY (Set.insert new points)

part1 :: IO ()
part1 = putStrLn . show . (\points -> simulate (Set.size points) (maximum . fmap snd . Set.toList $ points) points) . build =<< readFile "day14.txt"

part2 :: IO ()
part2 = putStrLn . show . (\points -> simulate2 (Set.size points) ((+2) . maximum . fmap snd . Set.toList $ points) points) . build =<< readFile "day14.txt"

main :: IO ()
main = part1
