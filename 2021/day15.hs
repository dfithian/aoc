#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.List.Extra (minimumOn, sortOn)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

findShortestPath :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int) -> Map (Int, Int) Int -> (Int, Int) -> Int
findShortestPath graph destination visited tentative current@(x, y) =
  let thisTentative = fromJust $ Map.lookup current tentative
      neighbors = filter (not . flip Set.member visited) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
      getNeighborTentative neighbor = case (Map.lookup neighbor graph, Map.lookup neighbor tentative) of
        (Nothing, _) -> Nothing
        (Just w, Nothing) -> Just (neighbor, w + thisTentative)
        (Just w, Just v) -> Just (neighbor, min v (w + thisTentative))
      neighborWeights = sortOn snd . catMaybes . fmap getNeighborTentative $ neighbors
      newTentative = foldr (uncurry Map.insert) tentative neighborWeights
      newVisited = Set.insert current visited
      unvisited = sortOn snd . catMaybes . fmap (\point -> (point,) <$> Map.lookup point newTentative) . Set.toList . Set.difference (Map.keysSet graph) $ newVisited
  in case current == destination of
    True -> thisTentative
    False -> findShortestPath graph destination newVisited newTentative (fst (head unvisited))

growGraph :: (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
growGraph (sizeX, sizeY) x0y0 =
  let nextW = \case
        9 -> 1
        n -> n + 1
      shiftRight = Map.fromList . fmap (\((x, y), w) -> ((x, y + sizeY + 1), nextW w)) . Map.toList
      shiftDown  = Map.fromList . fmap (\((x, y), w) -> ((x + sizeX + 1, y), nextW w)) . Map.toList
      x0y1 = shiftRight x0y0
      x0y2 = shiftRight x0y1
      x0y3 = shiftRight x0y2
      x0y4 = shiftRight x0y3
      x1y0 = shiftDown x0y0
      x1y1 = shiftDown x0y1
      x1y2 = shiftDown x0y2
      x1y3 = shiftDown x0y3
      x1y4 = shiftDown x0y4
      x2y0 = shiftDown x1y0
      x2y1 = shiftDown x1y1
      x2y2 = shiftDown x1y2
      x2y3 = shiftDown x1y3
      x2y4 = shiftDown x1y4
      x3y0 = shiftDown x2y0
      x3y1 = shiftDown x2y1
      x3y2 = shiftDown x2y2
      x3y3 = shiftDown x2y3
      x3y4 = shiftDown x2y4
      x4y0 = shiftDown x3y0
      x4y1 = shiftDown x3y1
      x4y2 = shiftDown x3y2
      x4y3 = shiftDown x3y3
      x4y4 = shiftDown x3y4
  in mconcat 
    [ x0y0, x0y1, x0y2, x0y3, x0y4
    , x1y0, x1y1, x1y2, x1y3, x1y4
    , x2y0, x2y1, x2y2, x2y3, x2y4
    , x3y0, x3y1, x3y2, x3y3, x3y4
    , x4y0, x4y1, x4y2, x4y3, x4y4
    ]

getFileContents :: FilePath -> IO (Map (Int, Int) Int)
getFileContents fp = do
  xss <- lines <$> readFile fp
  pure . Map.fromList . mconcat . zipWith (\i -> zipWith (\j x -> ((i, j), read [x])) [0..]) [0..] $ xss

main :: IO ()
main = do
  graph <- getFileContents "day15.txt"
  let dimensions = maximum $ Map.keys graph
      bigGraph = growGraph dimensions graph
      shortestPath = findShortestPath graph dimensions (Set.singleton (0, 0)) (Map.singleton (0, 0) 0) (0, 0)
      shortestPath2 = findShortestPath bigGraph (maximum $ Map.keys bigGraph) (Set.singleton (0, 0)) (Map.singleton (0, 0) 0) (0, 0)
  putStrLn $ show shortestPath
  putStrLn $ show shortestPath2
