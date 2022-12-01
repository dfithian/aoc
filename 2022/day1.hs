#!/usr/bin/env stack
-- stack --resolver lts-19 script

import Data.List (sort, sum)

part1 :: IO ()
part1 =
  putStrLn . show . maximum . snd . foldr (\next (cur, acc) -> if next == "" then (0, cur:acc) else (read next + cur, acc)) (0, []) . lines 
    =<< readFile "day1.txt"

part2 :: IO ()
part2 =
  putStrLn . show . sum . take 3 . reverse . sort . snd . foldr (\next (cur, acc) -> if next == "" then (0, cur:acc) else (read next + cur, acc)) (0, []) . lines 
    =<< readFile "day1.txt"

main :: IO ()
main = part2
