#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Prelude (read)
import Control.Monad (fail)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

data PasswordPolicy = PasswordPolicy Int Int Char
  deriving (Eq, Ord, Show)
newtype Password = Password String
  deriving (Eq, Ord, Show)
data PolicyAndPassword = PolicyAndPassword PasswordPolicy Password
  deriving (Eq, Ord, Show)

runParser :: Atto.Parser PolicyAndPassword -> ByteString -> IO PolicyAndPassword
runParser p = either fail pure . Atto.parseOnly p

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

isAlpha :: Word8 -> Bool
isAlpha w = (w >= 65 && w <= 90) || (w >= 97 && w <= 122)

spaces :: Atto.Parser ()
spaces = void $ Atto.many' (Atto.string " ")

digit :: Atto.Parser Int
digit = read . C8.unpack <$> (spaces *> Atto.takeWhile1 isDigit) <* spaces

alpha :: Atto.Parser Char
alpha = (spaces *> (toEnum . fromEnum <$> Atto.satisfy isAlpha)) <* spaces

policyParser :: Atto.Parser PasswordPolicy
policyParser = PasswordPolicy <$> digit <*> (Atto.skipWhile ((==) 45) *> digit) <*> (spaces *> alpha)

passwordParser :: Atto.Parser Password
passwordParser = Password . C8.unpack <$> (spaces *> (Atto.takeWhile isAlpha <* spaces))

policyAndPasswordParser :: Atto.Parser PolicyAndPassword
policyAndPasswordParser = PolicyAndPassword <$> policyParser <*> (Atto.skipWhile ((==) 58) *> passwordParser)

isValid :: PolicyAndPassword -> Bool
isValid (PolicyAndPassword (PasswordPolicy lo hi c) (Password p)) =
  let n = length . filter ((==) c) $ p
  in n >= lo && n <= hi

main :: IO ()
main = do
  passwordsAndPolicies <- traverse (runParser policyAndPasswordParser . encodeUtf8) .  lines
    =<< readFileUtf8 "day2.txt"
  let valids = filter isValid passwordsAndPolicies
  putStrLn
    $  "Got " <> tshow (length passwordsAndPolicies) <> " total passwords"
    <> ", " <> tshow (length valids) <> " valid passwords"
