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
        case (x == y, x + y) of
          (False, 2020) -> [x * y]
          _ -> []
  putStrLn $ tshow bad
