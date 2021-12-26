#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Control.Arrow (first)
import Data.Function (fix)
import Data.Map (Map)
import Data.Semigroup (Max(Max), getMax)
import qualified Data.Map as Map

data Direction = East | South
  deriving (Eq, Ord)

instance Show Direction where
  show = \case
    East -> ">"
    South -> "v"

showHerd :: (Int, Int) -> Map (Int, Int) Direction -> String
showHerd (maxI, maxJ) state =
  concatMap (\(i, j) -> (if j == 0 then ('\n':) else id) . maybe "." show . Map.lookup (i, j) $ state)
    $ [(i, j) | i <- [0..maxI], j <- [0..maxJ]]

moveHerd :: (Int, Int) -> Map (Int, Int) Direction -> Map (Int, Int) Direction
moveHerd (maxI, maxJ) state =
  let assocs = Map.assocs state
      moveEast ((i, j), direction) accum = case direction == East of
        True ->
          let coord = (i, if j >= maxJ then 0 else j + 1)
          in case Map.lookup coord state of
            Nothing -> Map.insert coord East accum
            Just _ -> Map.insert (i, j) East accum
        False -> accum
      moveSouth ((i, j), direction) accum = case direction == South of
        True ->
          let coord = (if i >= maxI then 0 else i + 1, j)
          in case (Map.lookup coord accum, Map.lookup coord state) of
            (Nothing, Nothing) -> Map.insert coord South accum
            (Nothing, Just East) -> Map.insert coord South accum
            _ -> Map.insert (i, j) South accum
        False -> accum
  in foldr moveSouth (foldr moveEast mempty assocs) assocs

getFileContents :: FilePath -> IO ((Int, Int), Map (Int, Int) Direction)
getFileContents fp =
  first getMax
    . mconcat
    . mconcat
    . fmap ( \(i, xs) ->
        fmap ( \(j, x) -> case x of
          '.' -> (Max (i, j), Map.empty)
          '>' -> (Max (i, j), Map.singleton (i, j) East)
          'v' -> (Max (i, j), Map.singleton (i, j) South)
        )
        . zip [0..]
        $ xs
      )
    . zip [0..]
    . lines
    <$> readFile fp

main :: IO ()
main = do
  (bounds, initial) <- getFileContents "day25.txt"
  let once = moveHerd bounds initial
      numIterations = flip fix (1, initial, once) $ \f (n, prev, curr) -> case prev == curr of
        True -> n
        False -> f (n + 1, curr, moveHerd bounds curr)
  putStrLn $ show numIterations
