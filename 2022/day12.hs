#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

import Distribution.Simple.Utils (safeHead)
import Data.List (foldl', sort)
import "containers" Data.Map (Map, (!))
import qualified "containers" Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

import Debug.Trace

type Waypoint = ((Int, Int), Int, Int)

build :: String -> ((Int, Int), (Int, Int), Map (Int, Int) Int)
build = foldr (\(row, s) acc -> foldr (\(col, c) acc' -> go row col c acc') acc . zip [0..] $ s) ((0, 0), (0, 0), mempty) . zip [0..] . lines
  where
    go row col c (start, end, xs) = case c of
      'S' -> ((row, col), end, Map.insert (row, col) 0 xs)
      'E' -> (start, (row, col), Map.insert (row, col) 25 xs)
      _ -> (start, end, Map.insert (row, col) (fromEnum c - fromEnum 'a') xs)

choice :: Int -> Map (Int, Int) Int -> Set (Int, Int) -> Waypoint -> Maybe Waypoint
choice maxN map seen (pos, elev, n) =
  case Map.lookup pos map of
    Just elev' | abs (elev - elev') <= 1 && n < maxN && not (Set.member pos seen) -> Just (pos, elev', n + 1)
    _ -> Nothing

choices :: Int -> Map (Int, Int) Int -> Set (Int, Int) -> Waypoint -> [Waypoint]
choices maxN map seen ((x, y), elev, n) = catMaybes . fmap (\pos -> choice maxN map seen (pos, elev, n)) $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

insertWaypoint :: Waypoint -> [Waypoint] -> [Waypoint]
insertWaypoint y@(_, _, n) = \case
  [] -> [y]
  x@(_, _, n'):xs -> case n < n' of
    True -> y:x:xs
    False -> x:(insertWaypoint y xs)

chopWaypoints :: Int -> [Waypoint] -> [Waypoint]
chopWaypoints n = \case
  x@(_, _, n'):xs -> case n < n' of
    True -> []
    False -> x:(chopWaypoints n xs)

move :: (Int, Int) -> Int -> Map (Int, Int) Int -> Set (Int, Int) -> [Waypoint] -> Int
move end maxN map seen waypoints = case waypoints of
  [] -> maxN
  next@(pos, elev, n):rest -> case Set.member pos seen of
    True -> move end maxN map seen rest
    False ->
      let (maxN', rest') = case (pos == end, n < maxN) of
            (True, True) -> (n, chopWaypoints n rest)
            (True, False) -> (maxN, rest)
            (False, _) -> (maxN, foldr insertWaypoint rest $ choices maxN map seen next)
       in trace (show (elev, n, pos)) $ move end maxN' map (Set.insert pos seen) rest'

part1 :: IO ()
part1 = do
  (start, end, map) <- build <$> readFile "day12.txt"
  putStrLn . show . move end maxBound map mempty $ [(start, map ! start, 0)]

main :: IO ()
main = part1
