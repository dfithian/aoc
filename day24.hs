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

data Dir = E | SE | SW | W | NW | NE
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

parseLine :: [Char] -> IO [Dir]
parseLine = \ case
  [] -> pure []
  'e':xs -> (E:) <$> parseLine xs
  'w':xs -> (W:) <$> parseLine xs
  's':'e':xs -> (SE:) <$> parseLine xs
  's':'w':xs -> (SW:) <$> parseLine xs
  'n':'e':xs -> (NE:) <$> parseLine xs
  'n':'w':xs -> (NW:) <$> parseLine xs
  other -> fail $ "Can't parse " <> other

flipColor :: Color -> Color
flipColor = \ case
  Black -> White
  White -> Black

addOne, subOne :: Int -> Int
addOne = (+) 1
subOne x = x - 1

getCoord :: [Dir] -> (Int, Int)
getCoord = \ case
  [] -> (0, 0)
  E:xs -> bimap (addOne . addOne) id $ getCoord xs
  W:xs -> bimap (subOne . subOne) id $ getCoord xs
  SE:xs -> bimap addOne subOne $ getCoord xs
  SW:xs -> bimap subOne subOne $ getCoord xs
  NE:xs -> bimap addOne addOne $ getCoord xs
  NW:xs -> bimap subOne addOne $ getCoord xs

tileFloor :: [[Dir]] -> Map (Int, Int) Color
tileFloor = foldl' (\ acc next -> let coord = getCoord next in insertMap coord (flipColor . fromMaybe White . lookup coord $ acc) acc) mempty

neighborCoords :: (Int, Int) -> Set (Int, Int)
neighborCoords (x, y) = setFromList [(x - 2, y), (x - 1, y + 1), (x + 1, y + 1), (x + 2, y), (x + 1, y - 1), (x - 1, y - 1)]

numBlackNeighbors :: Map (Int, Int) Color -> (Int, Int) -> Int
numBlackNeighbors m = length . filter ((==) (Just Black)) . map (flip lookup m) . setToList . neighborCoords

iterateDay :: Map (Int, Int) Color -> Map (Int, Int) Color
iterateDay m = foldr f mempty (unions . map neighborCoords . keys $ m)
  where
    f next acc =
      let color = case fromMaybe White . lookup next $ m of
            White -> if numBlackNeighbors m next == 2 then Black else White
            Black -> let n = numBlackNeighbors m next in if n == 0 || n > 2 then White else Black
      in insertMap next color acc

main :: IO ()
main = do
  dirs <- traverse parseLine . lines . unpack =<< readFileUtf8 "day24.txt"
  putStrLn . tshow . length . filter ((==) Black . snd) . mapToList . foldr (\ _ acc -> iterateDay acc) (tileFloor dirs) $ [1..100]
