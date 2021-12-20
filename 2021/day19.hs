#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.Containers.ListUtils (nubOrd)
import Data.Function (fix)
import Data.List (group, sort)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Tuple.Extra (uncurry3)
import qualified Data.Set as Set

data Rotate = X | Y | Z

data Point = P Int Int Int
  deriving (Eq, Ord)

data Distance = D Int Int Int
  deriving (Eq, Ord)

sub :: Point -> Point -> Distance
sub (P x1 y1 z1) (P x2 y2 z2) = D (x1 - x2) (y1 - y2) (z1 - z2)

add :: Point -> Distance -> Point
add (P x1 y1 z1) (D x2 y2 z2) = P (x1 + x2) (y1 + y2) (z1 + z2)

euclidean :: Point -> Point -> Int
euclidean (P x1 y1 z1) (P x2 y2 z2) = (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

manhattan :: Distance -> Distance -> Int
manhattan (D x1 y1 z1) (D x2 y2 z2) = abs (x1 - x2 + y1 - y2 + z1 - z2)

magic :: Int
magic = 12

safeHead :: [a] -> Maybe a
safeHead = \case
  x:_ -> Just x
  [] -> Nothing

-- |Determine whether two regions overlap by identifying if there is a magic number of points which have the same distance from their neighbors.
-- Run this before 'distanceOverlap', since that's expensive.
regionsOverlap :: [(Point, Set Int)] -> [(Point, Set Int)] -> Bool
regionsOverlap fixed variable =
  let isOverlap ptV ptF = Set.size (Set.intersection (snd ptV) (snd ptF)) >= magic
      hasOverlap ptV = not . null . filter (isOverlap ptV) $ fixed
  in (>= magic) . length . filter hasOverlap $ variable

-- |Find the adjustment distance between two regions. Assumes 'regionsOverlap' or some other metric has said that these regions overlap.
distanceOverlap :: [(Point, Set Int)] -> [(Point, Set Int)] -> Maybe Distance
distanceOverlap fixed variable =
  let getOverlapDistance ptV ptF = if Set.size (Set.intersection (snd ptV) (snd ptF)) >= magic then [sub (fst ptF) (fst ptV)] else []
      findFixedDist ptV = concatMap (getOverlapDistance ptV) $ fixed
  in fmap head . safeHead . filter ((>= magic) . length) . group . sort . concatMap findFixedDist $ variable

-- |Get distances to each neighbor within a region of points.
distancesToEachNeighbor :: [Point] -> [(Point, Set Int)]
distancesToEachNeighbor pts = fmap (\pt -> (pt, Set.fromList $ fmap (euclidean pt) pts)) pts

-- |Permute region by all 24 possible rotations.
rotate :: [(Point, Set Int)] -> [[(Point, Set Int)]]
rotate pts =
  let rotatePoint (P ( x) ( y) ( z)) = \case
              X -> P ( x) ( z) (-y)
              Y -> P (-z) ( y) ( x)
              Z -> P ( y) (-x) ( z)
      allRotations =
        [ [], [X], [X, X], [X, X, X], [Y], [Y, Y, Y]
        , [Z], [Z, X], [Z, X, X], [Z, X, X, X], [Z, Y], [Z, Y, Y, Y]
        , [Z, Z], [Z, Z, X], [Z, Z, X, X], [Z, Z, X, X, X], [Z, Z, Y], [Z, Z, Y, Y, Y]
        , [Z, Z, Z], [Z, Z, Z, X], [Z, Z, Z, X, X], [Z, Z, Z, X, X, X], [Z, Z, Z, Y], [Z, Z, Z, Y, Y, Y]
        ]
  in (\rs -> fmap (\(pt, dist) -> (foldr (flip rotatePoint) pt rs, dist)) pts) <$> allRotations
  
-- |Fit two regions together and return the rotation and adjustment distance of the second argument which fits into the first.
fit :: [(Point, Set Int)] -> [(Point, Set Int)] -> Maybe ([Point], Distance)
fit fixed variable =
  case regionsOverlap fixed variable of
    True -> Just . head . catMaybes . fmap (\pts -> (fst <$> pts,) <$> distanceOverlap fixed pts) . rotate $ variable
    False -> Nothing

-- |Reduce the problem, returning the number of unique points and maximum manhattan distance, respectively.
reduce :: [(Int, [Point])] -> (Int, Int)
reduce ((_, region):regions) =
  flip fix (region, [], regions) $ \f -> \case
    (uniques, distances, []) -> 
      ( length uniques
      , maximum . fmap (uncurry manhattan) $ [ (x, y) | x <- distances, y <- distances ]
      )
    (uniqueAcc, distancesAcc, (nextId, next):rest) -> case fit (distancesToEachNeighbor uniqueAcc) (distancesToEachNeighbor next) of
      Nothing -> f (uniqueAcc, distancesAcc, rest <> [(nextId, next)])
      Just (rotation, distance) ->
        let newAcc = nubOrd $ fmap (flip add distance) rotation <> uniqueAcc
        in f (newAcc, distance:distancesAcc, rest)

getFileContents :: FilePath -> IO [(Int, [Point])]
getFileContents fp =
  reverse . fix go . ([],) . lines <$> readFile fp
  where
    go f (acc, xs) = case span (not . null) xs of
      ([], _) -> acc
      (rawId:rawCoords, rest) ->
        let next =
             ( read . head . drop 2 . words $ rawId
             , fmap (\rawCoord -> uncurry3 P $ read ("(" <> rawCoord <> ")")) rawCoords
             )
        in f (next:acc, dropWhile null rest)

main :: IO ()
main = do
  regions <- getFileContents "day19.txt"
  let (uniquePoints, maxDistance) = reduce regions
  putStrLn $ show uniquePoints
  putStrLn $ show maxDistance
