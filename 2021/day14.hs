#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.Function (fix)
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

step :: Map (Char, Char) Char -> Map (Char, Char) Int -> Map (Char, Char) Int
step rules state = foldr go mempty $ Map.keys state
  where
    go p@(p1, p3) acc =
      let n = fromJust $ Map.lookup p state
          p2 = fromJust $ Map.lookup p rules
      in foldr (\q -> Map.insertWith (+) q n) acc [(p1, p2), (p2, p3)]

countChars :: Char -> Map (Char, Char) Int -> Int
countChars final state =
  let counts = sort . fmap snd . Map.toList . foldr (\(p, n) -> Map.insertWith (+) (fst p) n) (Map.singleton final 1) . Map.toList $ state
  in last counts - head counts

runRules :: Int -> Char -> Map (Char, Char) Char -> Map (Char, Char) Int -> Int
runRules iterations final rules initial =
  flip fix (iterations, initial) $ \f -> \case
    (0, state) -> countChars final state
    (n, state) -> f (n - 1, step rules state)

getFileContents :: FilePath -> IO ([Char], Map (Char, Char) Char)
getFileContents fp = do
  ([initial], _:rawRules) <- span ((/=) "") . lines <$> readFile fp
  let rules = Map.fromList . fmap ((\[[p1, p2], _, [c]] -> ((p1, p2), c)) . words) $ rawRules
  pure (initial, rules)

main :: IO ()
main = do
  (input, rules) <- getFileContents "day14.txt"
  let initial = foldr (\p -> Map.insertWith (+) p 1) mempty . zip input . drop 1 $ input
      part1 = runRules 10 (last input) rules initial
      part2 = runRules 40 (last input) rules initial
  putStrLn $ show part1
  putStrLn $ show part2
