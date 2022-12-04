#!/usr/bin/env stack
-- stack --resolver lts-19 script

import Data.List.Split (splitOn)

parse :: String -> ((Int, Int), (Int, Int))
parse x = 
  let [l, r] = splitOn "," x
      [lStart, lEnd] = splitOn "-" l
      [rStart, rEnd] = splitOn "-" r
  in ((read lStart, read lEnd), (read rStart, read rEnd))

contains :: (Int, Int) -> (Int, Int) -> Bool
contains (x1, x2) (y1, y2) = x1 <= y1 && y2 <= x2

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (x1, x2) (y1, y2) = (x1 <= y1 && y1 <= x2) || (x1 <= y2 && y2 <= x2)

part1 :: IO ()
part1 =
  putStrLn . show . length . filter (\(x, y) -> contains x y || contains y x) . fmap parse . lines
    =<< readFile "day4.txt"

part2 :: IO ()
part2 =
  putStrLn . show . length . filter (\(x, y) -> overlaps x y || overlaps y x) . fmap parse . lines
    =<< readFile "day4.txt"

main :: IO ()
main = part2
