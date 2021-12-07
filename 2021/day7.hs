#!/usr/bin/env stack
-- stack --resolver lts script

import Data.List (sort)
import Data.List.Split (splitOn)

-- for constant cost, it's the median
getChoicesConstant :: [Int] -> [Int]
getChoicesConstant xs =
  let len = length xs
  in case len `mod` 2 of
    0 -> take 1 $ drop (len `div` 2) xs
    _ -> take 2 $ drop (len `div` 2) xs

-- i really don't know how to find this, so try every possibility :P
getChoicesGauss :: [Int] -> [Int]
getChoicesGauss xs = [minimum xs..maximum xs]
  
sumFuelConstant :: Int -> [Int] -> Int
sumFuelConstant n = sum . fmap (\x -> abs (x - n))

sumFuelGauss :: Int -> [Int] -> Int
sumFuelGauss n = sum . fmap (\x -> let y = abs (x - n) in y * (y + 1) `div` 2)

getMinFuelCost 
  :: (Int -> [Int] -> Int) 
  -> ([Int] -> [Int])
  -> [Int]
  -> Int
getMinFuelCost sumFuel getChoices xs =
  minimum . fmap (\x -> sumFuel x xs) . getChoices $ xs

getFileContents :: FilePath -> IO [Int]
getFileContents fp = do
  rawPositions:_ <- lines <$> readFile fp
  pure $ read <$> splitOn "," rawPositions

main :: IO ()
main = do
  positions <- sort <$> getFileContents "day7.txt"
  let fuelCostConstant = getMinFuelCost sumFuelConstant getChoicesConstant positions
      fuelCostGauss = getMinFuelCost sumFuelGauss getChoicesGauss positions
  putStrLn $ show fuelCostConstant
  putStrLn $ show fuelCostGauss
