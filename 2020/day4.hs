#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Data.Char (isDigit)
import Data.Set (isSubsetOf)
import Data.Text (replace, splitOn)

newtype Passport = Passport (Map Text Text)

required :: Set Text
required = setFromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

mkPassport :: Text -> Either Text Passport
mkPassport rawPassport = validate =<< traverse parseOne (words rawPassport)
  where
    validate xs =
      let ks = setFromList $ fst <$> xs
          missing = difference required ks
      in case null missing of
        True -> Right . Passport . mapFromList $ xs
        False -> Left $ "Missing required fields " <> intercalate ", " (setToList missing) <> " in passport " <> rawPassport
    parseOne x = case splitOn ":" x of
      [k, v] -> case k of
        "byr" -> case readMay v of
          Just (x :: Int) | x >= 1920 && x <= 2002 -> Right (k, v)
          _ -> Left $ "Invalid birth year " <> v <> " for " <> x
        "iyr" -> case readMay v of
          Just (x :: Int) | x >= 2010 && x <= 2020 -> Right (k, v)
          _ -> Left $ "Invalid issue year " <> v <> " for " <> x
        "eyr" -> case readMay v of
          Just (x :: Int) | x >= 2020 && x <= 2030 -> Right (k, v)
          _ -> Left $ "Invalid expiration year " <> v <> " for " <> x
        "hgt" -> case (stripSuffix "cm" v, stripSuffix "in" v) of
          (Just x, _) -> case readMay x of
            Just (n :: Int) | n >= 150 && n <= 193 -> Right (k, v)
            _ -> Left $ "Invalid height " <> v <> " for " <> x
          (_, Just x) -> case readMay x of
            Just (n :: Int) | n >= 59 && n <= 76 -> Right (k, v)
            _ -> Left $ "Invalid height " <> v <> " for " <> x
          _ -> Left $ "Invalid height " <> v <> " for " <> x
        "hcl" -> case unpack v of
          '#':rest | null $ difference (setFromList rest) (asSet $ setFromList "0123456789abcdef") -> Right (k, v)
          _ -> Left $ "Invalid hair color " <> v <> " for " <> x
        "ecl" -> case v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] of
          True -> Right (k, v)
          False -> Left $ "Invalid eye color " <> v <> " for " <> x
        "pid" -> let strV = unpack v in case all isDigit strV && length strV == 9 of
          True -> Right (k, v)
          False -> Left $ "Invalid passport id " <> v <> " for " <> x
        _ -> Right (k, v)
      _ -> Left $ "Invalid passport: " <> x

main :: IO ()
main = do
  rawPassports <- map (replace "\n" " ") . splitOn "\n\n"
    <$> readFileUtf8 "day4.txt"
  let passports = map mkPassport rawPassports
      valid = rights passports
      invalid = lefts passports
  -- putStrLn $ unlines rawPassports
  putStrLn $ "Got " <> tshow (length passports) <> " total passports, and " <> tshow (length valid) <> " valid ones"
  -- putStrLn $ unlines invalid
