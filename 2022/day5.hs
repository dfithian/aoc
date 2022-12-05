#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

import Data.List (drop, foldl', take, transpose)
import "containers" Data.Map (Map, (!))
import qualified "containers" Data.Map.Strict as Map
import Data.Maybe (catMaybes)

parseStack :: String -> [Maybe Char]
parseStack xs = case splitAt 3 xs of
  ("   ", cs) -> Nothing:(parseStack (drop 1 cs))
  (['[', c, ']'], cs) -> (Just c):(parseStack (drop 1 cs))
  ([], _) -> []

parseMove :: String -> (Int, Int, Int)
parseMove x = case words x of
  ["move", n, "from", x, "to", y] -> (read n, read x, read y)

parse :: [String] -> (Map Int [Char], [(Int, Int, Int)])
parse xs =
  let (rawStacks, rawMoves) = span (not . null) xs
      stacks = Map.fromList . zip [1..] . fmap catMaybes . transpose . fmap parseStack . take (length rawStacks - 1) $ rawStacks
      moves = fmap parseMove . drop 1 $ rawMoves
   in (stacks, moves)

move :: Bool -> Map Int [Char] -> (Int, Int, Int) -> Map Int [Char]
move shouldReverse xs (n, x, y) =
  let stack = take n $ xs ! x
      f = if shouldReverse then reverse else id
   in Map.update (Just . drop n) x . Map.update (Just . (f stack <>)) y $ xs

top :: Map Int [Char] -> String
top = fmap (head . snd) . Map.toList

part1 :: IO ()
part1 = do
  (stacks, moves) <- parse . lines <$> readFile "day5.txt"
  putStrLn . top . foldl' (move True) stacks $ moves

part2 :: IO ()
part2 = do
  (stacks, moves) <- parse . lines <$> readFile "day5.txt"
  putStrLn . top . foldl' (move False) stacks $ moves

main :: IO ()
main = part1
