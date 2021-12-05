#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE TupleSections #-}

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

shadeMap :: (((Int, Int), (Int, Int)) -> [(Int, Int)]) -> [((Int, Int), (Int, Int))] -> Map (Int, Int) Int
shadeMap getCoords = foldr go mempty
  where
    go next acc = foldr go2 acc (getCoords next)
    go2 next acc = Map.insertWith (+) next 1 acc

getLinearCoords :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getLinearCoords ((x1, x2), (y1, y2)) = case (compare x1 y1, compare x2 y2) of
  (EQ, EQ) -> [(x1, x2)]
  (EQ, LT) -> (x1,) <$> [x2..y2]
  (EQ, GT) -> (x1,) <$> [y2..x2]
  (LT, EQ) -> (,x2) <$> [x1..y1]
  (GT, EQ) -> (,x2) <$> [y1..x1]
  _ -> []

getAllCoords :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getAllCoords ((x1, x2), (y1, y2)) = case (compare x1 y1, compare x2 y2) of
  (EQ, EQ) -> [(x1, x2)]
  (EQ, LT) -> (x1,) <$> [x2..y2]
  (EQ, GT) -> (x1,) <$> [y2..x2]
  (LT, EQ) -> (,x2) <$> [x1..y1]
  (GT, EQ) -> (,x2) <$> [y1..x1]
  (LT, LT) -> zip [x1..y1] [x2..y2]
  (LT, GT) -> zip [x1..y1] [x2,(x2-1)..y2]
  (GT, LT) -> zip [x1,(x1-1)..y1] [x2..y2]
  (GT, GT) -> zip [x1,(x1-1)..y1] [x2,(x2-1)..y2]

countIntersections :: Map (Int, Int) Int -> Int
countIntersections = Map.size . Map.filter (>= 2)

getFileContents :: FilePath -> IO [((Int, Int), (Int, Int))]
getFileContents fp = do
  rawSegments <- lines <$> readFile fp
  pure $ rawSegments <&> \rawSegment ->
    let x:_:y:[] = words rawSegment
        x1:x2:[] = splitOn "," x
        y1:y2:[] = splitOn "," y
    in ((read x1, read x2), (read y1, read y2))

main :: IO ()
main = do
  segments <- getFileContents "day5.txt"
  let shadedLinear = shadeMap getLinearCoords segments
      shadedAll = shadeMap getAllCoords segments
      intersectionsLinear = countIntersections shadedLinear
      intersectionsAll = countIntersections shadedAll
  putStrLn $ show intersectionsLinear
  putStrLn $ show intersectionsAll
