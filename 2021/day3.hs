#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Control.Monad (fail)
import Data.Bifunctor (bimap)
import Data.List ((!!), partition, transpose)

toInt :: [Bool] -> Int
toInt = fst . foldr go (0, 1)
  where
    go next (acc, mult) = (acc + (if next then mult else 0), mult * 2)

getGreek :: ((Int, Int) -> Bool) -> [[Bool]] -> [Bool]
getGreek f = foldr go mempty . transpose
  where
    go next acc = (f (bimap length length (partition id next))):acc

getGamma, getEpsilon :: [[Bool]] -> [Bool]
getGamma = getGreek (uncurry (>=))
getEpsilon = getGreek (uncurry (<))

foldGreek :: ([[Bool]] -> [Bool]) -> [[Bool]] -> [Bool]
foldGreek f xs =
  let go next = \case
        [] -> []
        [y] -> [y]
        ys -> filter (\y -> f ys !! next == y !! next) ys
  in head $ foldl (flip go) xs [0..(length (transpose xs) - 1)]

getO2, getCO2 :: [[Bool]] -> [Bool]
getO2 = foldGreek getGamma
getCO2 = foldGreek getEpsilon

getFileContents :: FilePath -> IO [[Bool]]
getFileContents fp =
  fmap (fmap ((==) (1 :: Int) . read . flip (:) [])) . lines <$> readFile fp

main :: IO ()
main = do
  bs <- getFileContents "day3.txt"
  let gamma = toInt $ getGamma bs
      epsilon = toInt $ getEpsilon bs
      o2 = toInt $ getO2 bs
      co2 = toInt $ getCO2 bs
  putStrLn $ "Gamma " <> show gamma
  putStrLn $ "Epsilon " <> show epsilon
  putStrLn $ "Product " <> show (gamma * epsilon)
  putStrLn $ "O2 " <> show o2
  putStrLn $ "CO2 " <> show co2
  putStrLn $ "Product " <> show (o2 * co2)
