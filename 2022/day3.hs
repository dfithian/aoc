#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}

import Data.Char (isLower)
import Data.List (intersect, sum)

score :: Char -> Int
score c = case isLower c of
  True -> fromEnum c - fromEnum 'a' + 1
  False -> fromEnum c - fromEnum 'A' + 1 + 26

intersectAll :: Eq a => [[a]] -> [a]
intersectAll = \case
  [] -> []
  x:xs -> foldr intersect x xs

part1 :: IO ()
part1 =
  putStrLn . show . sum . fmap (\xs -> score . head . uncurry intersect . splitAt (length xs `div` 2) $ xs) . lines 
    =<< readFile "day3.txt"

part2 :: IO ()
part2 =
  putStrLn . show . sum
    . fmap (score . head . intersectAll) . uncurry (:)
    . foldr (\next (cur, acc) -> if length cur == 3 then ([next], cur:acc) else (next:cur, acc)) ([], [])
    . lines 
    =<< readFile "day3.txt"

main :: IO ()
main = part2
