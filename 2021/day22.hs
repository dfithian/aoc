#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

data Region = Region (Int, Int) (Int, Int) (Int, Int)
  deriving (Eq, Ord, Show)

bounds :: Region
bounds = Region (-50, 50) (-50, 50) (-50, 50)

countPoints :: Set (Int, Int, Int) -> (Bool, Region) -> Set (Int, Int, Int)
countPoints acc (isOn, region) =
  let f = if isOn then Set.insert else Set.delete
      Region (x1, x2) (y1, y2) (z1, z2) = region
      Region (minX, maxX) (minY, maxY) (minZ, maxZ) = bounds
      pts = [ (x, y, z) | x <- [(max minX x1)..(min maxX x2)], y <- [(max minY y1)..(min maxY y2)], z <- [(max minZ z1)..(min maxZ z2)] ]
  in foldr f acc pts

getFileContents :: FilePath -> IO [(Bool, Region)]
getFileContents fp = do
  let parse l =
        let (instruction, _:rawCoords) = span ((/=) ' ') l
            [[x1, x2], [y1, y2], [z1, z2]] = fmap (splitOn ".." . drop 2) $ splitOn "," rawCoords
        in (instruction == "on", Region (read x1, read x2) (read y1, read y2) (read z1, read z2))
  fmap parse . lines <$> readFile fp

main :: IO ()
main = do
  regions <- getFileContents "day22.txt"
  putStrLn $ show $ Set.size $ foldl countPoints mempty regions
