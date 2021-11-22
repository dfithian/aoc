#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude
import Control.Monad (fail)

main :: IO ()
main = do
  numbers :: [Int] <- traverse (\ n -> maybe (fail $ show n <> ": not a number") pure $ readMay n) .  lines
    =<< readFileUtf8 "day1.txt"
  let bad = headMay $ do
        x <- numbers
        y <- numbers
        z <- numbers
        case (x == y || y == z || x == z, x + y + z) of
          (False, 2020) -> [x * y * z]
          _ -> []
  putStrLn $ tshow bad
