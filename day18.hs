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

data Operand = OperandPrim Int | OperandExp Expression
  deriving (Eq, Show)

data Expression = ExpressionAdd Operand Operand | ExpressionMult Operand Operand | ExpressionParen Expression
  deriving (Eq, Show)

ints :: [Char]
ints = ['0'..'9']

parseNumber :: String -> IO Int
parseNumber str = case readMay str of
  Nothing -> fail $ str <> " not a number"
  Just i -> pure i

parseOperator :: String -> IO (Operand -> Operand -> Expression)
parseOperator = \ case
  "+" -> pure ExpressionAdd
  "*" -> pure ExpressionMult
  str -> fail $ str <> " not an operator"

findMatchingParen :: String -> IO (String, String)
findMatchingParen str = do
  let inner i = \ case
        ')':cs -> case i <= 1 of
          True -> do
            pure ("", dropWhile ((==) ' ') cs)
          False -> do
            (remainder, done) <- inner (i - 1) cs
            pure (')':remainder, done)
        '(':cs -> do
          (remainder, done) <- inner (i + 1) cs
          pure ('(':remainder, done)
        c:cs -> do
          (remainder, done) <- inner i cs
          pure (c:remainder, done)
        [] -> fail "Ran out of input before finding matching paren"
  inner 1 str

reverseStr :: String -> String
reverseStr = foldr flipParen mempty . reverse
  where
    flipParen next acc = case next of
      ')' -> '(':acc
      '(' -> ')':acc
      _ -> next:acc

parseExpression :: String -> IO Expression
parseExpression = \ case
  '(':as -> do
    (as2, as3) <- findMatchingParen as
    expression <- ExpressionParen <$> parseExpression as2
    case words as3 of
      operatorStr:as4 -> do
        operator <- parseOperator operatorStr
        case as4 of
          [] -> fail "Ran out of input 1"
          numberStr:[] -> do
            number <- parseNumber numberStr
            pure $ operator (OperandExp expression) (OperandPrim number)
          _ -> do
            expression2 <- parseExpression $ unwords as4
            pure $ operator (OperandExp expression) (OperandExp expression2)
      [] -> pure expression
  bs@(i:_) | i `elem` ints -> case words bs of
    numberStr:operatorStr:bs3 -> do
      number <- parseNumber numberStr
      operator <- parseOperator operatorStr
      case bs3 of
        [] -> fail "Ran out of input 3"
        number2Str:[] -> do
          number2 <- parseNumber number2Str
          pure $ operator (OperandPrim number) (OperandPrim number2)
        _ -> do
          expression <- parseExpression $ unwords bs3
          pure $ operator (OperandPrim number) (OperandExp expression)
    cs -> fail $ unwords cs <> " not a valid expression 1"
  ds -> fail $ ds <> " not a valid expression 2"

evaluateOperand :: Operand -> Int
evaluateOperand = \ case
  OperandPrim i -> i
  OperandExp e -> evaluateExpression e

getLeft :: Operand -> (Int, Int -> Int)
getLeft = \ case
  OperandPrim i -> (i, id)
  OperandExp (ExpressionParen e) -> (evaluateExpression e, id)
  OperandExp (ExpressionAdd lhs rhs) ->
    let x = evaluateOperand lhs
        (y, combine) = getLeft rhs
    in (x, \ w -> combine (w + y))
  OperandExp (ExpressionMult lhs rhs) ->
    let x = evaluateOperand lhs
        y = evaluateOperand rhs
    in (x, \ z -> (z * y))

evaluateExpression :: Expression -> Int
evaluateExpression = \ case
  ExpressionParen e -> evaluateExpression e
  ExpressionAdd lhs rhs ->
    let x = evaluateOperand lhs
        (y, combine) = getLeft rhs
    in combine (x + y)
  ExpressionMult lhs rhs ->
    let x = evaluateOperand lhs
        y = evaluateOperand rhs
    in (x * y)

main :: IO ()
main = do
  es <- traverse (parseExpression . reverseStr) . lines . unpack =<< readFileUtf8 "day18.txt"
  putStrLn $ tshow $ sum $ evaluateExpression <$> es
