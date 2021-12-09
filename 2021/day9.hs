#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.List (intersect, sort, transpose)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- part 1

isLower :: (Maybe Int, Int, Maybe Int) -> Bool
isLower = \case
  (Nothing, x, Nothing) -> True
  (Nothing, x, Just y2) -> x < y2
  (Just y1, x, Nothing) -> x < y1
  (Just y1, x, Just y2) -> x < y1 && x < y2

findLowPointsLine :: (Int -> (Int, Int)) -> [Int] -> [(Int, (Int, Int))]
findLowPointsLine mkCoord xs =
  fmap (\((_, x, _), pos) -> (x, mkCoord pos))
    . filter (isLower . fst)
    . zip (zip3 (Nothing:(Just <$> xs)) xs ((Just <$> drop 1 xs) <> [Nothing]))
    $ [0..]

findLowPointsRow :: Int -> [Int] -> [(Int, (Int, Int))]
findLowPointsRow row = findLowPointsLine (row,)

findLowPointsCol :: Int -> [Int] -> [(Int, (Int, Int))]
findLowPointsCol col = findLowPointsLine (,col)

findLowPointsGrid :: [[Int]] -> [(Int, (Int, Int))]
findLowPointsGrid xss =
  let lowRows = mconcat $ zipWith (flip findLowPointsRow) xss [0..]
      lowCols = mconcat $ zipWith (flip findLowPointsCol) (transpose xss) [0..]
  in lowRows `intersect` lowCols

-- part 2

findAdjacents :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, (Int, Int)) -> Set (Int, (Int, Int))
findAdjacents xmap (i, j) acc =
  let adjacents =
        flip Set.difference acc
          . Set.fromList 
          . filter ((< 9) . fst) 
          . catMaybes 
          . fmap (\coord -> (,coord) <$> Map.lookup coord xmap)
          $ [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
  in foldr (findAdjacents xmap . snd) (acc <> adjacents) (Set.toList adjacents)

findBasins :: [[Int]] -> [(Int, (Int, Int))] -> [Set (Int, (Int, Int))]
findBasins xss lowPoints =
  let xmap = foldr (\(i, xs) acc -> foldr (\(j, x) -> Map.insert (i, j) x) acc xs) mempty . zip [0..] . fmap (zip [0..]) $ xss
  in flip fmap lowPoints $ \pt@(_, coord) -> findAdjacents xmap coord (Set.singleton pt)

getFileContents :: FilePath -> IO [[Int]]
getFileContents fp = fmap (fmap (read . flip (:) [])) . lines <$> readFile fp

main :: IO ()
main = do
  depths <- getFileContents "day9.txt"
  let lowPoints = findLowPointsGrid depths
      riskLevelSum = sum $ ((+) 1 . fst) <$> lowPoints
      basinProduct = product . take 3 . reverse . sort . fmap Set.size . findBasins depths $ lowPoints
  putStrLn $ show riskLevelSum
  putStrLn $ show basinProduct
