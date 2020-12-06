#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Control.Monad (fail)
import Data.Text (replace, splitOn)

main :: IO ()
main = do
  answers <- map (words . unpack . replace "\n" " ") . splitOn "\n\n"
    <$> readFileUtf8 "day6.txt"
  let f next = \ case
        Nothing -> Just $ setFromList next
        Just acc -> Just $ intersect (setFromList next) acc
      uniqueYesCount = map (length . setToList . asSet . fromMaybe mempty . foldr f Nothing) answers
  putStrLn $ "Got unique yes counts: \n" <> intercalate "\n" (tshow <$> uniqueYesCount)
  putStrLn $ "Sum: " <> tshow (sum uniqueYesCount)
