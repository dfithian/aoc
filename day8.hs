#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Control.Monad (fail)
import Control.Monad.State (evalStateT, get, put)
import Data.List ((!!))
import Data.Text (splitOn)
import Prelude (read)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

data Instruction
  = Nop
  | Acc
  | Jmp
  deriving (Eq, Show)

data Line = Line Instruction Int
  deriving (Eq, Show)

instruction :: Atto.Parser Instruction
instruction = (Nop <$ Atto.string "nop") <|> (Acc <$ Atto.string "acc") <|> (Jmp <$ Atto.string "jmp")

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

spaces :: Atto.Parser ()
spaces = void $ Atto.takeWhile ((==) 32)

number :: Atto.Parser Int
number = Atto.take 1 >>= \ case
  "-" -> negate . read . C8.unpack <$> Atto.takeWhile isDigit
  "+" -> read . C8.unpack <$> Atto.takeWhile isDigit
  _ -> fail "No sign available"

line :: Atto.Parser Line
line = Line <$> instruction <*> (spaces *> number)

evalOne :: Map Int (Bool, Line) -> (Bool, Int)
evalOne is = runIdentity . flip evalStateT (0, is, 0) $
  let inner = do
        (idx, instr, acc) <- get
        case lookup idx instr of
          Nothing -> pure (True, acc)
          Just (True, _) -> pure (False, acc)
          Just (False, Line Nop n) -> put (idx + 1, insertMap idx (True, Line Nop n) instr, acc) >> inner
          Just (False, Line Acc n) -> put (idx + 1, insertMap idx (True, Line Acc n) instr, acc + n) >> inner
          Just (False, Line Jmp n) -> put (idx + n, insertMap idx (True, Line Jmp n) instr, acc) >> inner
  in inner

main :: IO ()
main = do
  instructions <- either fail (pure . asMap . mapFromList . zip [0..] . zip (repeat False)) . traverse (Atto.parseOnly line . encodeUtf8) . lines
    =<< readFileUtf8 "day8.txt"
  accum <- flip evalStateT 0 $
    let inner = do
          idx <- get
          case lookup idx instructions of
            Nothing -> fail "Completed loop without finding anything"
            Just (_, Line Nop n) -> case evalOne (insertMap idx (False, Line Jmp n) instructions) of
              (False, _) -> put (idx + 1) >> inner
              (True, acc) -> pure acc
            Just (_, Line Acc _) -> put (idx + 1) >> inner
            Just (_, Line Jmp n) -> case evalOne (insertMap idx (False, Line Nop n) instructions) of
              (False, _) -> put (idx + 1) >> inner
              (True, acc) -> pure acc
    in inner
  putStrLn $ "Accumulator: " <> tshow accum
