#!/usr/bin/env stack
-- stack --resolver lts script

import Data.List (nub, transpose)
import Data.List.Split (chunksOf, splitOn)

isSolved :: [Int] -> [[Int]] -> Bool
isSolved hits xss =
  let isRow = any (all (flip elem hits)) xss
      isColumn = any (all (flip elem hits)) $ transpose xss
  in isRow || isColumn

getScore :: [Int] -> [[Int]] -> Int
getScore hits xss =
  let winning = last hits
      unmarked = sum . filter (not . flip elem hits) . mconcat $ xss
  in winning * unmarked

runTurn :: Int -> [Int] -> [[[Int]]] -> [([Int], [[Int]])]
runTurn turn hits = mconcat . fmap go
  where
    turnHits = take turn hits
    go xss = if isSolved turnHits xss then [(turnHits, xss)] else []

getWinners :: [Int] -> [[[Int]]] -> [([Int], [[Int]])]
getWinners hits xsss = mconcat $ fmap go [0..(length hits)]
  where
    go turn = runTurn turn hits xsss

runGame :: [Int] -> [[[Int]]] -> Int
runGame hits xsss = uncurry getScore . head . getWinners hits $ xsss

runLosingGame :: [Int] -> [[[Int]]] -> Int
runLosingGame hits xsss = 
  let winners = getWinners hits xsss
      lastWinningBoard = last . nub . fmap snd $ winners
  in uncurry getScore . head . dropWhile ((/=) lastWinningBoard . snd) $ winners
  where
    go turn = runTurn turn hits xsss

getFileContents :: FilePath -> IO ([Int], [[[Int]]])
getFileContents fp = do
  rawMoves:_:rawBoards <- lines <$> readFile fp
  let moves = read <$> splitOn "," rawMoves
      boards = fmap (fmap (fmap read) . fmap words . take 5) $ chunksOf 6 rawBoards
  pure (moves, boards)

main :: IO ()
main = do
  (hits, xsss) <- getFileContents "day4.txt"
  let score = runGame hits xsss
      losingScore = runLosingGame hits xsss
  putStrLn $ "Score is " <> show score
  putStrLn $ "Losing score is " <> show losingScore
