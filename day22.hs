#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude
import Control.Monad (fail)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

newline :: Atto.Parser ()
newline = void $ Atto.string "\n"

number :: Atto.Parser Int
number = do
  cs <- C8.unpack <$> Atto.takeWhile1 isDigit
  maybe (fail $ cs <> " is not a number") pure $ readMay cs

playerParser :: Int -> Atto.Parser [Int]
playerParser n = Atto.string ("Player " <> C8.pack (show n) <> ":\n") *> Atto.sepBy1 number newline

fileParser :: Atto.Parser ([Int], [Int])
fileParser = (,) <$> playerParser 1 <*> (newline *> newline *> playerParser 2)

playRound :: (NonEmpty Int, NonEmpty Int) -> ([Int], [Int])
playRound = \ case
  (x:|xs, y:|ys) -> case (x <= length xs && y <= length ys) of
    True -> case playGame [] (take x xs, take y ys) of
      Left _ -> (xs <> [x, y], ys)
      Right _ -> (xs, ys <> [y, x])
    False -> case x > y of
      True -> (xs <> [x, y], ys)
      False -> (xs, ys <> [y, x])

playGame :: [([Int], [Int])] -> ([Int], [Int]) -> Either Int Int
playGame previous = \ case
  (xs, []) -> Left . sum . zipWith (*) [1..] . reverse $ xs
  ([], ys) -> Right . sum . zipWith (*) [1..] . reverse $ ys
  round@(x:xs, y:ys) -> case round `elem` previous of
    True -> Left 1
    False -> playGame (round:previous) $ playRound (x:|xs, y:|ys)

main :: IO ()
main = do
  (player1, player2) <- either fail pure . Atto.parseOnly fileParser =<< readFile "day22.txt"
  putStrLn . tshow . playGame [] $ (player1, player2)
