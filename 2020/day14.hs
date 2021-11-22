#!/usr/bin/env stack
-- stack --resolver lts script

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
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

data MaskChar
  = MaskChar0
  | MaskChar1
  | MaskCharX
  deriving (Eq, Show)

data Instruction
  = Mask [MaskChar]
  | Mem Int Int
  deriving (Eq, Show)

maskCharParser :: Atto.Parser MaskChar
maskCharParser = Atto.take 1 >>= \ case
  "0" -> pure MaskChar0
  "1" -> pure MaskChar1
  "X" -> pure MaskCharX
  c -> fail $ "Invalid mask char " <> C8.unpack c

maskParser :: Atto.Parser Instruction
maskParser = Mask <$> (void (Atto.string "mask = ") *> Atto.many' maskCharParser)

isDigit :: Word8 ->Bool
isDigit w = w >= 48 && w <= 57

digit :: Atto.Parser Int
digit = (\ s -> maybe (fail $ "Not a digit: " <> s) pure $ readMay s) . C8.unpack =<< Atto.takeWhile1 isDigit

brackets :: Atto.Parser p -> Atto.Parser p
brackets p = (Atto.string "[" *> p) <* Atto.string "]"

memParser :: Atto.Parser Instruction
memParser = Mem <$> (void (Atto.string "mem") *> brackets digit) <*> (void (Atto.string " = ") *> digit)

instructionParser :: Atto.Parser Instruction
instructionParser = maskParser <|> memParser

instructionsParser :: Atto.Parser [Instruction]
instructionsParser = Atto.many' (instructionParser <* void (Atto.string "\n"))

convert :: Int -> [Bool]
convert x =
  let this = case x `mod` 2 of
        0 -> False
        _ -> True
  in case x `div` 2 of
    0 -> [this]
    n -> this:(convert n)

unconvert :: [Bool] -> Int
unconvert = foldr f 0
  where
    f next acc = case next of
      True -> acc * 2 + 1
      False -> acc * 2

applyMask :: [MaskChar] -> [Bool] -> [MaskChar]
applyMask mask value =
  flip map (mask `zip` (value <> repeat False)) $ \ case
    (MaskChar0, False) -> MaskChar0
    (MaskChar0, True) -> MaskChar1
    (MaskChar1, _) -> MaskChar1
    (MaskCharX, _) -> MaskCharX

extrapolateMask :: [MaskChar] -> [[Bool]]
extrapolateMask = \ case
  [] -> [[]]
  MaskChar0:ms -> (False:) <$> extrapolateMask ms
  MaskChar1:ms -> (True:) <$> extrapolateMask ms
  MaskCharX:ms -> do
    bs <- extrapolateMask ms
    [True:bs, False:bs]

runInstruction :: ([MaskChar], Map [Bool] [Bool]) -> Instruction -> ([MaskChar], Map [Bool] [Bool])
runInstruction (mask, memory) next = case next of
  Mask newMask -> (reverse newMask, memory)
  Mem addr value -> (mask, f addr value)
  where
    f addr value =
      let binValue = convert value
          binAddrs = extrapolateMask $ applyMask mask $ convert addr
      in foldr (\ next acc -> insertMap next binValue acc) memory binAddrs

main :: IO ()
main = do
  instructions <- either fail pure . Atto.parseOnly instructionsParser =<< readFile "day14.txt"
  putStrLn . tshow . sum . map (unconvert . snd) . mapToList . snd . foldl' runInstruction ([], mempty) $ instructions
