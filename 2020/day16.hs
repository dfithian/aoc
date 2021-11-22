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
import Data.List ((!!))
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as Set

data Rule = Rule Text (Int, Int) (Int, Int)
  deriving (Eq, Show)

data Ticket = Ticket [Int]
  deriving (Eq, Show)

data File = File [Rule] Ticket [Ticket]
  deriving (Eq, Show)

isAlpha :: Word8 -> Bool
isAlpha w = (w >= 65 && w <= 90) || (w >= 97 && w <= 122) || w == 32

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

colon :: Atto.Parser ()
colon = void $ Atto.string ":"

colonSpace :: Atto.Parser ()
colonSpace = colon <* void (Atto.string " ")

dash :: Atto.Parser ()
dash = void $ Atto.string "-"

newline :: Atto.Parser ()
newline = void $ Atto.string "\n"

alpha :: Atto.Parser Text
alpha = decodeUtf8 <$> Atto.takeWhile1 isAlpha

number :: Atto.Parser Int
number = do
  cs <- C8.unpack <$> Atto.takeWhile1 isDigit
  maybe (fail $ cs <> " is not a number") pure $ readMay cs

range :: Atto.Parser (Int, Int)
range = (,) <$> (number <* dash) <*> number

rule :: Atto.Parser Rule
rule = Rule <$> (alpha <* colonSpace) <*> (range <* Atto.string " or ") <*> range

ticket :: Atto.Parser Ticket
ticket = Ticket <$> Atto.sepBy1 number (Atto.string ",")

myTicket :: Atto.Parser Ticket
myTicket = void (Atto.string "your ticket:\n") *> ticket

otherTickets :: Atto.Parser [Ticket]
otherTickets = void (Atto.string "nearby tickets:\n") *> Atto.sepBy1 ticket newline

file :: Atto.Parser File
file = File <$> Atto.sepBy1 rule newline <*> (newline *> newline *> myTicket) <*> (newline *> newline *> otherTickets)

hasValidTicketValues :: [Rule] -> Ticket -> Bool
hasValidTicketValues rules (Ticket is) =
  let allRuleRanges = flip concatMap rules $ \ (Rule _ (x, y) (z, w)) -> [(x, y), (z, w)]
      matchesRule i = any (\ (x, y) -> i >= x && i <= y) allRuleRanges
  in all matchesRule is

ticketRuleMatches :: [Rule] -> Ticket -> Map Int (Set Text)
ticketRuleMatches rules (Ticket is) = mapFromList . flip map (is `zip` [0..]) $ \ (i, n) ->
  (n, foldr (\ (Rule name (x, y) (z, w)) acc -> if (i >= x && i <= y) || (i >= z && i <= w) then insertSet name acc else acc) mempty rules)

main :: IO ()
main = do
  File rules (Ticket myIs) others <- either fail pure . Atto.parseOnly file =<< readFile "day16.txt"
  let valids = filter (hasValidTicketValues rules) others
      matches = unionsWith intersect $ ticketRuleMatches rules <$> valids
  rulePositions <- map fst . foldlM ( \ (acc, usedNames) (n, names) ->
    case headMay . setToList . difference names $ usedNames of
      Nothing -> fail $ "Ran out of positions for " <> show (n, names) <> ", " <> show (acc, usedNames) <> ", " <> show matches
      Just name -> pure ((n, name):acc, insertSet name usedNames)
    ) (mempty, mempty) . sortOn (Set.size . snd) . mapToList $ matches
  putStrLn . tshow . product
    . map ((myIs !!) . fst)
    . filter (isPrefixOf "departure" . snd)
    $ rulePositions
