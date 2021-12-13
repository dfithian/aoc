#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.Char (isUpper)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

data Cave = Start | End | Big String | Small String
  deriving (Eq, Ord, Show)

data Revisit = Never | Once Cave | Twice
  deriving (Eq, Ord, Show)

buildPart1Paths :: Map Cave (Set Cave) -> Set Cave -> Cave -> [[Cave]]
buildPart1Paths graph visited cave =
  let toVisit = Map.findWithDefault Set.empty cave graph \\ visited
      recurse newVisited = fmap ((:) cave) $ foldr (\next acc -> buildPart1Paths graph newVisited next <> acc) mempty toVisit
  in case (cave, Set.null toVisit) of
    (End, _) -> [[cave]]
    (_, True) -> []
    (Big _, _) -> recurse visited
    _ -> recurse (Set.insert cave visited)

buildPart2Paths :: Map Cave (Set Cave) -> Set Cave -> Revisit -> Cave -> [[Cave]]
buildPart2Paths graph visited revisit cave =
  let toVisit = Map.findWithDefault Set.empty cave graph \\ visited
      recurse params = fmap ((:) cave) $ nub $ foldr (\next acc -> foldr (\(v, r) acc2 -> buildPart2Paths graph v r next <> acc2) acc params) mempty toVisit
  in case (cave, revisit, Set.null toVisit) of
    (End, _, _) -> [[cave]]
    (_, _, True) -> []
    (Big _, _, _) -> recurse [(visited, revisit)]
    (Small _, Never, _) -> recurse [(visited, Once cave), (Set.insert cave visited, Never)]
    (Small _, Once this, _) | this == cave -> recurse [(Set.insert cave visited, Twice)]
    _ -> recurse [(Set.insert cave visited, revisit)]

getFileContents :: FilePath -> IO (Map Cave (Set Cave))
getFileContents fp =
  foldr (\[x, y] -> Map.insertWith (<>) x (Set.singleton y) . Map.insertWith (<>) y (Set.singleton x)) mempty
    . fmap (fmap go . splitOn "-")
    . lines 
    <$> readFile fp
  where
    go = \case
      "start" -> Start
      "end" -> End
      big | all isUpper big -> Big big
      small -> Small small

main :: IO ()
main = do
  graph <- getFileContents "day12.txt"
  let part1Paths = buildPart1Paths graph (Set.singleton Start) Start
      part2Paths = buildPart2Paths graph (Set.singleton Start) Never Start
  putStrLn $ show $ length part1Paths
  putStrLn $ show $ length part2Paths
