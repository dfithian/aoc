#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE PackageImports #-}

import Data.Foldable (traverse_)
import Data.List (foldl', sum)
import Data.List.Split (chunksOf)
import "containers" Data.Map (Map, (!))
import qualified "containers" Data.Map.Strict as Map

data Instruction = Add Int | Noop
  deriving (Show)

data State = State Int Int
  deriving (Show)

data History = History [Int]
  deriving (Show)

data Crt = Crt [Bool]
  deriving (Show)

parse :: String -> [Instruction]
parse s = case words s of
  ["noop"] -> [Noop]
  ["addx", n] -> [Noop, Add $ read n]

cycleOnce :: Instruction -> (State, History) -> (State, History)
cycleOnce i (State x y, History hs) =
  let z = case i of
        Noop -> y
        Add w -> y + w
   in (State y z, History (x:hs))

complete :: (State, History) -> History
complete (State x _, History hs) = History (x:hs)

run :: [Instruction] -> History
run = complete . foldl' (flip cycleOnce) (State 1 1, History [])

historyAt :: History -> Int -> Int
historyAt (History hs) n = n * head (drop n . reverse $ hs)

historiesAt :: History -> [Int] -> Int
historiesAt history = sum . fmap (historyAt history)

drawCrt :: Crt -> String
drawCrt (Crt pixels) = unlines . chunksOf 40 . fmap (\b -> if b then '#' else ' ') $ pixels

crt :: History -> Crt
crt (History hs) = Crt . mconcat . fmap (fmap (\(n, x) -> abs (n - x) <= 1) . zip [0..]) . chunksOf 40 . drop 1 . reverse $ hs

part1 :: IO ()
part1 = putStrLn . show . flip historiesAt [20, 60, 100, 140, 180, 220] . run . concatMap parse . lines =<< readFile "day10.txt"

part2 :: IO ()
part2 = putStrLn . drawCrt . crt . run . concatMap parse . lines =<< readFile "day10.txt"

main :: IO ()
main = part2
