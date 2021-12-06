#!/usr/bin/env stack
-- stack --resolver lts script

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

iterateDay :: Int -> Map Int Int -> Map Int Int
iterateDay _ = foldr go mempty . Map.toList
  where
    go (k, v) acc = case k of
      0 -> Map.insertWith (+) 6 v . Map.insertWith (+) 8 v $ acc
      n -> Map.insertWith (+) (n-1) v acc
      
getFileContents :: FilePath -> IO [Int]
getFileContents fp = do
  rawAges:_ <- lines <$> readFile fp
  pure $ read <$> splitOn "," rawAges

main :: IO ()
main = do
  ages <- getFileContents "day6.txt"
  let input = foldr (\age -> Map.insertWith (+) age 1) mempty ages
      finalAges = sum . Map.elems . foldr iterateDay input $ [1..256]
  putStrLn $ show finalAges
