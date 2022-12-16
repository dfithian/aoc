#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

import Data.List (foldl', transpose)
import "containers" Data.Map (Map)
import qualified "containers" Data.Map.Strict as Map

import Debug.Trace

data Interval = Interval Int Int

data Occupy = Sensor Int | Beacon Int | Range Interval

instance Show Interval where
  show (Interval start end) = "[" <> show start <> ".." <> show end <> "]"

count :: [Occupy] -> Int
count = sum . Map.elems . foldl' f mempty
  where
    f acc = \case
      Sensor x -> Map.insert x 1 acc
      Beacon x -> Map.insert x 0 acc
      Range (Interval start end) -> foldl' (\acc' next -> Map.insertWith (\_ old -> old) next 1 acc') acc [start..end]

build :: String -> [((Int, Int), (Int, Int))]
build = fmap (f . words) . lines
  where
    f ["Sensor", "at", sx, sy, "closest", "beacon", "is", "at", bx, by] =
      ( ( read . takeWhile ((/=) ',') . drop 2 $ sx
        , read . takeWhile ((/=) ':') . drop 2 $ sy
        )
      , ( read . takeWhile ((/=) ',') . drop 2 $ bx
        , read . drop 2 $ by
        )
      )

neighbors :: (Int, Int) -> (Int, Int) -> [(Int, Interval)]
neighbors (sx, sy) (bx, by) =
  let maxDist = abs (sx - bx) + abs (sy - by)
      intervals i =
        [ (sy + i, Interval (sx - maxDist + i) (sx + maxDist - i))
        , (sy - i, Interval (sx - maxDist + i) (sx + maxDist - i))
        ]
      inBounds (y, interval) = fst input == y
   in filter inBounds . concatMap intervals $ [0..maxDist]

insert :: Int -> Occupy -> Map Int [Occupy] -> Map Int [Occupy]
insert y o = Map.insertWith (<>) y [o]

coverage :: [((Int, Int), (Int, Int))] -> Map Int [Occupy]
coverage = foldl' (flip f) mempty
  where
    g acc (y, interval) = insert y (Range interval) acc
    f ((sx, sy), (bx, by)) =
      insert sy (Sensor sx)
        . insert by (Beacon bx)
        . flip (foldl' g) (neighbors (sx, sy) (bx, by))

input :: (Int, String)
-- input = (10, "day15-alt.txt")
input = (2000000, "day15.txt")

part1 :: IO ()
part1 = putStrLn . show . count . Map.findWithDefault mempty (fst input) . coverage . build =<< readFile (snd input)

main :: IO ()
main = part1
