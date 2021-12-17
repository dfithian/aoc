#!/usr/bin/env stack
-- stack --resolver lts script

import Data.Containers.ListUtils (nubOrd)
import Data.Function (fix)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

shootProbe :: Set (Int, Int) -> ((Int, Int) -> Bool) -> (Int, Int) -> Maybe ((Int, Int), Int)
shootProbe area missed initial =
  let made = flip Set.member area
  in flip fix ((0, 0), initial, 0) $ \f -> \((px, py), (vx, vy), maxY) ->
    case (made (px, py), missed (px, py)) of
      (True, _) -> Just (initial, maxY)
      (_, True) -> Nothing
      _ ->
        let newVx = max 0 (vx - 1)
            newVy = vy - 1
        in f ((px + newVx, py + newVy), (newVx, newVy), max maxY (py + newVy))

targetArea :: Set (Int, Int)
targetArea = Set.fromList [ (x, y) | x <- [281..311], y <- [(-74)..(-54)] ]
-- targetArea = Set.fromList [ (x, y) | x <- [20..30], y <- [(-10)..(-5)] ]

main :: IO ()
main = do
  let maxX = maximum . fmap fst . Set.toList $ targetArea
      minY = minimum . fmap snd . Set.toList $ targetArea
      missed (x, y) = x > maxX || y < minY
      (velocities, heights) = unzip . catMaybes . fmap (shootProbe targetArea missed) $ [ (x, y) | x <- [1..(2 * maxX)], y <- [(2 * (-maxX))..(2 * maxX)] ]
  putStrLn $ show $ maximum heights
  putStrLn $ show $ length $ nubOrd velocities
