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

doorRealPublicKey, cardRealPublicKey, doorAltPublicKey, cardAltPublicKey, publicKeySubjectNumber, modValue :: Int
doorRealPublicKey = 17786549
cardRealPublicKey = 7573546
doorAltPublicKey = 17807724
cardAltPublicKey = 5764801
publicKeySubjectNumber = 7
modValue = 20201227

applyLoop :: Int -> Int -> Int -> Int
applyLoop subjectNumber val = \ case
  0 -> val
  loop -> applyLoop subjectNumber ((val * subjectNumber) `mod` modValue) (loop - 1)

findLoopSize :: Int -> Int
findLoopSize publicKey =
  let inner val loop =
        let newVal = (val * publicKeySubjectNumber) `mod` modValue
        in if newVal == publicKey then loop else inner newVal (loop + 1)
  in inner 1 1

main :: IO ()
main = do
  let doorPublicKey = doorRealPublicKey
      cardPublicKey = cardRealPublicKey
      doorLoopSize = findLoopSize doorPublicKey
      cardLoopSize = findLoopSize cardPublicKey
  putStrLn . tshow . applyLoop cardPublicKey 1 $ doorLoopSize
