#!/usr/bin/env stack
-- stack --resolver lts-19 script

import Data.List (nub, zip5)

zip15 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [i] -> [j] -> [k] -> [l] -> [m] -> [n] -> [o] -> [(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)]
zip15 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) (j:js) (k:ks) (l:ls) (m:ms) (n:ns) (o:os) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o):(zip15 as bs cs ds es fs gs hs is js ks ls ms ns os)
zip15 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = []

score4 :: String -> Int
score4 x =
  let choices = zip5 [4..] x (drop 1 x) (drop 2 x) (drop 3 x)
      f (_, a, b, c, d) = [a, b, c, d] == nub [a, b, c, d]
      g (idx, _, _, _, _) = idx
   in head . fmap g . filter f $ choices

score14 :: String -> Int
score14 x =
  let choices = zip15 [14..] x (drop 1 x) (drop 2 x) (drop 3 x) (drop 4 x) (drop 5 x) (drop 6 x) (drop 7 x) (drop 8 x) (drop 9 x) (drop 10 x) (drop 11 x) (drop 12 x) (drop 13 x)
      f (_, a, b, c, d, e, f, g, h, i, j, k, l, m, n) = [a, b, c, d, e, f, g, h, i, j, k, l, m, n] == nub [a, b, c, d, e, f, g, h, i, j, k, l, m, n]
      g (idx, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = idx
   in head . fmap g . filter f $ choices

part1 :: IO ()
part1 = putStrLn . show . score4 . head . lines =<< readFile "day6.txt"

part2 :: IO ()
part2 = putStrLn . show . score14 . head . lines =<< readFile "day6.txt"

main :: IO ()
main = part2
