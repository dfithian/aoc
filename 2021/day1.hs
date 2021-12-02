#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  xs :: [Int] <- fmap read . lines <$> readFile "day1.txt"
  let l = last xs
      ys = zip3 (l:xs) xs (drop 1 xs)
      z:zs = fmap (\(x, y, z) -> x + y + z) ys
      n = snd $ foldl (\(prev, sum) next -> if next > prev then (next, sum + 1) else (next, sum)) (z, 0) zs
  putStrLn $ show n
