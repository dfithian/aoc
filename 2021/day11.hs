#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.Function (fix)
import Data.List (nub)
import Data.Map (Map)
import Data.Set ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

data Energy = Energy Int | Flashed
  deriving (Eq)

increase :: Energy -> Energy
increase = \case
  Energy 9 -> Flashed
  Energy n -> Energy (n + 1)
  Flashed -> Flashed

reset :: Energy -> Energy
reset = \case
  Flashed -> Energy 0
  x -> x

instance Show Energy where
  show = \case
    Energy n -> show n
    Flashed -> "*"

step :: (Map (Int, Int) Energy, Int, Int) -> (Map (Int, Int) Energy, Int, Int)
step (levels, numFlashes, stepNum) =
  let getAdjacents (i, j) = [(i-1, j-1), (i-1, j), (i-1, j+1), (i, j-1), (i, j+1), (i+1, j-1), (i+1, j), (i+1, j+1)]
      getFlashed = Map.keysSet . Map.filter ((==) Flashed)
      bumpAdjacents coord = flip (foldr (Map.adjust increase)) (getAdjacents coord)
      initialIncreased = Map.map increase levels
      initialFlashed = Set.toList $ getFlashed initialIncreased
      finished = flip fix (initialIncreased, mempty, initialFlashed) $ \f -> \case
        (acc, _, []) -> acc
        (acc, alreadyFlashed, newFlashes) ->
          let newLevels = foldr bumpAdjacents acc newFlashes
              newOldFlashes = Set.fromList newFlashes <> alreadyFlashed
              newNewFlashes = getFlashed newLevels \\ newOldFlashes
          in f (newLevels, newOldFlashes, Set.toList newNewFlashes)
      newNumFlashes = numFlashes + Map.size (Map.filter ((==) Flashed) finished)
  in (Map.map reset finished, newNumFlashes, stepNum + 1)

withTerminate 
  :: ((Map (Int, Int) Energy, Int, Int) -> Maybe a) 
  -> (Map (Int, Int) Energy, Int, Int) 
  -> a
withTerminate f = fix $ \g x -> case f x of
  Nothing -> g (step x)
  Just y -> y

getFileContents :: FilePath -> IO [[Int]]
getFileContents fp = fmap (fmap (read . flip (:) [])) . lines <$> readFile fp

main :: IO ()
main = do
  energyLevels <- (,0,0) . foldr (\(i, xs) acc -> foldr (\(j, x) -> Map.insert (i, j) (Energy x)) acc xs) mempty . zip [0..] . fmap (zip [0..]) 
    <$> getFileContents "day11.txt"
  let iterate100Times (_, output, n) = if n == 100 then Just output else Nothing
      goUntilAllFlashed (every, _, output) = if all ((==) (Energy 0)) (Map.elems every) then Just output else Nothing
      numFlashes = withTerminate iterate100Times energyLevels
      whenAllFlashed = withTerminate goUntilAllFlashed energyLevels
  putStrLn $ show numFlashes
  putStrLn $ show whenAllFlashed
