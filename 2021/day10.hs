#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.Either (lefts, rights)
import Data.List (sort)

getCorruptionScore :: Char -> Int
getCorruptionScore = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> error "invalid character!"

getCompletionScore :: Char -> Int
getCompletionScore = \case
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _ -> error "invalid character!"

getClosingChar :: Char -> Char
getClosingChar = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  _ -> error "invalid character!"

parseChunks :: ([Char], [Char]) -> Either Char [Char]
parseChunks = \case
  (prev, []) -> Right prev
  (prev, c:rest) | isOpener c -> parseChunks (c:prev, rest)
  (o:prev, c:rest) -> case getClosingChar o == c of
    True -> parseChunks (prev, rest)
    False -> Left c
  ([], rest) -> error ("Oh no! " <> show rest)
  where
    isOpener = flip elem ['(', '[', '{', '<']

getFileContents :: FilePath -> IO [[Char]]
getFileContents fp = lines <$> readFile fp

main :: IO ()
main = do
  code <- getFileContents "day10.txt"
  let chunks = parseChunks . ([],) <$> code
      corruptionScore = sum $ getCorruptionScore <$> lefts chunks
      completionScores = foldl (\acc next -> acc * 5 + getCompletionScore next) 0 . fmap getClosingChar <$> rights chunks
      completionScore = head . drop (length completionScores `div` 2) . sort $ completionScores
  putStrLn $ show corruptionScore
  putStrLn $ show completionScore
