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

data Space = Active | Inactive
  deriving (Eq, Show)

type Game = Map Int (Map Int (Map Int (Map Int Space)))

printGame :: Game -> IO ()
printGame game = do
  let printChar = \ case
        Active -> '#'
        Inactive -> '.'
      printRow = map (printChar . snd) . mapToList
      printSlice = unlines . map (printRow . snd) . mapToList
  for_ (mapToList game) $ \ (w, zGame) -> for (mapToList zGame) $ \ (z, slice) -> do
    putStrLn $ "z=" <> tshow z <> ", w=" <> tshow w
    putStrLn $ pack $ printSlice slice

-- |First is W, then Z, then X, then Y
parseGame :: [String] -> IO Game
parseGame = map (singletonMap 0 . singletonMap 0) . parseSlice
  where
    parseSlice xss = do
      spaces <- traverse parseRow xss
      let bound = length spaces `div` 2
      pure . mapFromList $ zip [-bound..bound] spaces
    parseRow xs = do
      spaces <- traverse parseChar xs
      let bound = length spaces `div` 2
      pure . mapFromList $ zip [-bound..bound] spaces
    parseChar = \ case
      '.' -> pure Inactive
      '#' -> pure Active
      c -> fail $ [c] <> " not a space"

getCoord :: Game -> (Int, Int, Int, Int) -> Maybe Space
getCoord game (x, y, z, w) = lookup y =<< lookup x =<< lookup z =<< lookup w game

insertCoord :: Game -> (Int, Int, Int, Int) -> Space -> Game
insertCoord game (x, y, z, w) space =
  let this = singletonMap w . singletonMap z . singletonMap x . singletonMap y $ space
  in unionWith (unionWith (unionWith (<>))) this game

neighborsOf :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
neighborsOf coord@(x, y, z, w) = filter (not . (==) coord) $
  [ (a, b, c, d)
  | a <- [(x - 1)..(x + 1)]
  , b <- [(y - 1)..(y + 1)]
  , c <- [(z - 1)..(z + 1)]
  , d <- [(w - 1)..(w + 1)]
  ]

getNeighborSpaceCount :: Game -> Space -> (Int, Int, Int, Int) -> Int
getNeighborSpaceCount game space coord =
  length . filter ((==) space) . catMaybes . map (getCoord game) . neighborsOf $ coord

iterate :: Game -> Game
iterate game =
  let wzBound = (length game + 2) `div` 2
      xyBound = (maybe 3 ((+) 2 . length) $ lookup 0 =<< lookup 0 game) `div` 2
      coords = [ (x, y, z, w) | x <- [-xyBound..xyBound], y <- [-xyBound..xyBound], z <- [-wzBound..wzBound], w <- [-wzBound..wzBound] ]
      buildGame next acc = case getCoord game next of
        Just Active ->
          let activeNeighbors = getNeighborSpaceCount game Active next
              nextSpace = if activeNeighbors `elem` [2, 3] then Active else Inactive
          in insertCoord acc next nextSpace
        _ ->
          let activeNeighbors = getNeighborSpaceCount game Active next
              nextSpace = if activeNeighbors == 3 then Active else Inactive
          in insertCoord acc next nextSpace
  in foldr buildGame mempty coords

countActive :: Game -> Int
countActive = length . concatMap (concatMap (concatMap (filter ((==) Active . snd) . mapToList . snd) . mapToList . snd) . mapToList . snd) . mapToList

main :: IO ()
main = do
  game <- parseGame . lines . unpack =<< readFileUtf8 "day17.txt"
  putStrLn . tshow . countActive . foldr (const iterate) game $ [1..6]

