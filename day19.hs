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
import Control.Monad (MonadFail, fail)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

data RuleIndex = RuleIndex Int
  deriving (Eq, Ord, Show)

data Rule = RulePrim Text | RuleComposite [[RuleIndex]]
  deriving (Eq, Ord, Show)

isAlpha :: Word8 -> Bool
isAlpha w = (w >= 65 && w <= 90) || (w >= 97 && w <= 122)

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

colon :: Atto.Parser ()
colon = void $ Atto.string ":"

colonSpace :: Atto.Parser ()
colonSpace = colon <* void (Atto.string " ")

newline :: Atto.Parser ()
newline = void $ Atto.string "\n"

space :: Atto.Parser ()
space = void $ Atto.string " "

alpha :: Atto.Parser Text
alpha = decodeUtf8 <$> Atto.takeWhile1 isAlpha

quotes :: Atto.Parser a -> Atto.Parser a
quotes p = (Atto.string "\"" *> p) <* Atto.string "\""

number :: Atto.Parser Int
number = do
  cs <- C8.unpack <$> Atto.takeWhile1 isDigit
  maybe (fail $ cs <> " is not a number") pure $ readMay cs

ruleIndexParser :: Atto.Parser RuleIndex
ruleIndexParser = RuleIndex <$> number

optionParser :: Atto.Parser [RuleIndex]
optionParser =
  (Atto.sepBy ruleIndexParser space) <|> (singleton <$> ruleIndexParser)

compositeParser :: Atto.Parser [[RuleIndex]]
compositeParser =
  (Atto.sepBy optionParser (Atto.string " | ")) <|> (singleton <$> optionParser)

ruleParser :: Atto.Parser Rule
ruleParser = (RulePrim <$> quotes alpha) <|> (RuleComposite <$> compositeParser)

lineParser :: Atto.Parser (RuleIndex, Rule)
lineParser = (,) <$> (RuleIndex <$> number <* colonSpace) <*> ruleParser

linesParser :: Atto.Parser (Map RuleIndex Rule)
linesParser = mapFromList <$> Atto.sepBy1 lineParser newline

valueParser :: Atto.Parser [Text]
valueParser = lines . decodeUtf8 <$> Atto.takeByteString

fileParser :: Atto.Parser (Map RuleIndex Rule, [Text])
fileParser = (,) <$> linesParser <*> (newline *> newline *> valueParser)

parseFile :: ByteString -> IO (Map RuleIndex Rule, [Text])
parseFile bs = case Atto.parse fileParser bs of
  Atto.Done _ x -> pure x
  other -> fail $ show other

hasCycle :: Map RuleIndex Rule -> RuleIndex -> Bool
hasCycle ruleMap idx = case lookup idx ruleMap of
  Just (RuleComposite alternatives) -> any (elem idx) alternatives
  _ -> False

matchRule :: (MonadFail m, MonadIO m) => Map RuleIndex Rule -> RuleIndex -> Text -> m [(Bool, Text)]
matchRule ruleMap idx str = case lookup idx ruleMap of
  Nothing -> fail $ "Rule " <> show idx <> " does not exist"
  Just rule -> case rule of
    RulePrim x -> case stripPrefix x str of
      Just rest -> pure [(True, rest)]
      _ -> pure [(False, str)]
    RuleComposite alternatives -> do
      let f acc next = case filter (not . null . snd) acc of
            [] -> pure [(False, "")]
            input -> do
              once <- filter fst . mconcat <$> traverse (matchRule ruleMap next . snd) input
              case hasCycle ruleMap next of
                False -> pure once
                True -> do
                  again <- filter fst . mconcat <$> traverse (matchRule ruleMap next . snd) once
                  pure . ordNub $ once <> again
          g rules = foldlM f [(True, str)] rules
      filter fst . mconcat <$> traverse g alternatives

staticRules :: Map RuleIndex Rule
staticRules = mapFromList
  [ (RuleIndex 8, RuleComposite [[RuleIndex 42], [RuleIndex 42, RuleIndex 8]])
  , (RuleIndex 11, RuleComposite [[RuleIndex 42, RuleIndex 31], [RuleIndex 42, RuleIndex 11, RuleIndex 31]])
  ]

isValid :: Map RuleIndex Rule -> RuleIndex -> Text -> IO (Text, Bool)
isValid rules idx value = do
  valid <- not . null . filter (\ (b, remaining) -> b && null remaining) <$> matchRule rules (RuleIndex 0) value
  pure (value, valid)

main :: IO ()
main = do
  (rawRules, values) <- either fail pure . Atto.parseOnly fileParser =<< readFile "day19.txt"
  let rules = staticRules <> rawRules
  matches <- traverse (isValid rules (RuleIndex 0)) values
  putStrLn . tshow . length . filter snd $ matches
