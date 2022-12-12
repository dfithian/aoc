#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

import Data.List (foldl', sort)
import Data.List.Split (chunksOf)
import "containers" Data.Map (Map, (!))
import qualified "containers" Data.Map.Strict as Map

data Monkey = Monkey Int [Int] (Int -> Int) Int Int Int

data State = State Int (Map Int Monkey)

sInspections :: State -> [Int]
sInspections (State _ monkeys) = (\(Monkey x _ _ _ _ _) -> x) <$> Map.elems monkeys

parseOne :: [String] -> (Int, Monkey)
parseOne [monkeyStr, startingStr, operationStr, testStr, trueStr, falseStr] = (num, Monkey 0 items operation divisibleBy ifTrue ifFalse)
  where
    num = read . takeWhile ((/=) ':') . head . drop 1 . words $ monkeyStr
    items = fmap (read . takeWhile ((/=) ',')) . drop 2 . words $ startingStr
    operation = case drop 3 (words operationStr) of
      ["old", "+", "old"] -> (*) 2
      ["old", "*", "old"] -> \n -> n * n
      ["old", "+", n] -> (+) (read n)
      ["old", "*", n] -> (*) (read n)
    divisibleBy = read . head . drop 3 . words $ testStr
    ifTrue = read . head . drop 5 . words $ trueStr
    ifFalse = read . head . drop 5 . words $ falseStr

build :: String -> State
build s =
  let parsed = fmap parseOne . chunksOf 6 . filter ((/=) "") . lines $ s
      lcd = product $ (\(_, Monkey _ _ _ x _ _) -> x) <$> parsed
   in State lcd (Map.fromList parsed)

turn :: State -> Int -> State
turn (State lcd monkeys) num =
  let (Monkey inspections items operation divisibleBy ifTrue ifFalse) = monkeys ! num
      monkey = Monkey (inspections + length items) [] operation divisibleBy ifTrue ifFalse
      f item monkeys' =
        let item' = operation item `mod` lcd
            num' = if item' `mod` divisibleBy == 0 then ifTrue else ifFalse
            Monkey inspections' items' operation' divisibleBy' ifTrue' ifFalse' = monkeys' ! num'
         in Map.insert num' (Monkey inspections' (items' <> [item']) operation' divisibleBy' ifTrue' ifFalse') monkeys'
   in State lcd . Map.insert num monkey . foldl' (flip f) monkeys $ items

runRounds :: Int -> State -> State
runRounds n state@(State _ monkeys) = case n of
  0 -> state
  _ -> runRounds (n - 1) . foldl' turn state . Map.keys $ monkeys

part1 :: IO ()
part1 = putStrLn . show . product . take 2 . reverse . sort . sInspections . runRounds 20 . build =<< readFile "day11.txt"

part2 :: IO ()
part2 = putStrLn . show . product . take 2 . reverse . sort . sInspections . runRounds 10000 . build =<< readFile "day11.txt"

main :: IO ()
main = part2
