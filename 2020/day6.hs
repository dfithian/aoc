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
  putStrLn . tshow . sum . map (length . setToList . asSet . fromMaybe mempty . foldr (\ next -> Just . maybe (setFromList next) (intersect (setFromList next))) Nothing . words . unpack . replace "\n" " ") . splitOn "\n\n"
    =<< readFileUtf8 "day6.txt"
