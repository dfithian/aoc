#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude
import Control.Monad (fail)
import Data.List ((!!), unfoldr)

data Atom
  = Tree
  | Space
  deriving (Eq)

type TreeMap = [[Atom]]

type Input = (TreeMap, (Int, Int), (Int, Int))

buildMap :: [String] -> Either String TreeMap
buildMap = traverse buildRow
  where
    buildRow = traverse buildAtom
    buildAtom = \ case
      '#' -> Right Tree
      '.' -> Right Space
      c -> Left $ [c] <> ": not a tree or space"

atomAtCoord :: Input -> Maybe Atom
atomAtCoord (atoms, (maxRows, maxCols), (x, y)) =
  case x >= maxRows of
    True -> Nothing
    False -> Just $ (atoms !! x) !! (y `mod` maxCols)

getNextAtom :: (Int, Int) -> Input -> Maybe (Atom, Input)
getNextAtom (slopeX, slopeY) input@(atoms, bounds, (x, y)) = (, newInput) <$> atomAtCoord input
  where
    newInput = (atoms, bounds, (x + slopeX, y + slopeY))

main :: IO ()
main = do
  treeMap <- either fail pure . buildMap . lines . unpack
    =<< readFileUtf8 "day3.txt"
  let atomsInPath = unfoldr (getNextAtom (1, 3)) (treeMap, (length treeMap, fromMaybe (error "Empty tree map") . headMay $ length <$> treeMap), (0, 0))
      numTreesInPath = length $ filter ((==) Tree) atomsInPath
  putStrLn $ "Got " <> tshow (length atomsInPath) <> " total cells, with " <> tshow numTreesInPath <> " trees"
