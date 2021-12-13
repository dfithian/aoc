#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Foldable (traverse_)
import qualified Data.Set as Set

data Fold = X Int | Y Int
  deriving (Eq, Ord, Show)

showGrid :: Set (Int, Int) -> [String]
showGrid grid =
  reverse . fmap reverse . uncurry (:) . foldl (flip go) mempty $ [ (x, y) | x <- [0..maxX], y <- [0..maxY] ]
  where
    allPts = Set.toList grid
    maxX = maximum $ fst <$> allPts
    maxY = maximum $ snd <$> allPts
    getChar x = if Set.member x grid then '#' else ' '
    go next (current, acc) = case snd next == 0 of
      True  -> ([getChar next], current:acc)
      False -> ((getChar next):current, acc)

foldAt :: Set (Int, Int) -> Fold -> Set (Int, Int)
foldAt grid instruction =
  let f = case instruction of
        X j -> \next@(x, _) -> go (x,) snd j next
        Y i -> \next@(_, y) -> go (,y) fst i next
  in foldr f mempty (Set.toList grid)
  where
    go tuple extract index next =
      let val = extract next
          combine = if val < index then (+) else (-)
      in Set.insert (tuple (combine index (val - index)))

getFileContents :: FilePath -> IO (Set (Int, Int), [Fold])
getFileContents fp = do
  let splitCoord l = case splitOn "," l of [x, y] -> (read y, read x)
      splitFold l = case splitOn "=" . head . drop 2 . words $ l of
        ["x", x] -> X (read x)
        ["y", y] -> Y (read y)
  (coords, _:folds) <- span ((/=) "") . lines <$> readFile fp
  pure (Set.fromList $ splitCoord <$> coords, splitFold <$> folds)

main :: IO ()
main = do
  (grid, folds) <- getFileContents "day13.txt"
  let firstGrid = foldAt grid (head folds)
      finalGrid = foldl foldAt grid folds
  putStrLn $ show $ Set.size firstGrid
  traverse_ putStrLn $ showGrid finalGrid
