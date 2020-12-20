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
import Control.Lens (set, ix)
import Control.Monad (fail)
import Data.List ((!!))
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as C8

data Tile = Tile
  { tileId :: Int
  , tileData :: [[Char]]
  }
  deriving (Eq, Ord, Show)

data Side = SideTop | SideBottom | SideLeft | SideRight
  deriving (Eq, Ord, Show, Enum, Bounded)

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

colon :: Atto.Parser ()
colon = void $ Atto.string ":"

newline :: Atto.Parser ()
newline = void $ Atto.string "\n"

number :: Atto.Parser Int
number = do
  cs <- C8.unpack <$> Atto.takeWhile1 isDigit
  maybe (fail $ cs <> " is not a number") pure $ readMay cs

idParser :: Atto.Parser Int
idParser = Atto.string "Tile " *> (number <* colon <* newline)

tileParser :: Atto.Parser Tile
tileParser = Tile <$> idParser <*> Atto.sepBy1 inner newline
  where
    inner = C8.unpack <$> Atto.takeWhile1 (\ w -> w == 35 || w == 46)

tilesParser :: Atto.Parser [Tile]
tilesParser = Atto.sepBy1 tileParser (newline <* newline)

otherSide :: Side -> Side
otherSide = \ case
  SideTop -> SideBottom
  SideBottom -> SideTop
  SideLeft -> SideRight
  SideRight -> SideLeft

getBorder :: Tile -> Side -> [Char]
getBorder (Tile _ tile) = \ case
  SideTop -> tile !! 0
  SideBottom -> tile !! (length tile - 1)
  SideLeft -> map (!! 0) tile
  SideRight -> map (!! (length tile - 1)) tile

flipTile :: [[Char]] -> [[Char]]
flipTile tile = reverse <$> tile

rotateTile :: [[Char]] -> Int -> [[Char]]
rotateTile tile = \ case
  0 -> tile
  1 -> rotate
  n -> rotateTile rotate (n - 1)
  where
    getElem row col = (tile !! (length tile - 1 - col)) !! row
    getRow row = foldr (\ next acc -> (getElem row next):acc) mempty [0..(length tile - 1)]
    rotate = foldr (\ next acc -> (getRow next):acc) mempty [0..(length tile - 1)]

getChoices :: [[Char]] -> [[[Char]]]
getChoices tile = ordNub $ do
  x <- rotateTile tile <$> [0..3]
  [x, flipTile x]

tileMatchesBorder :: [Char] -> Side -> Tile -> Maybe Tile
tileMatchesBorder border side (Tile i inner) =
  let f choice = if getBorder choice side == border then Just choice else Nothing
  in foldr (\ next acc -> acc <|> f next) Nothing $ Tile i <$> getChoices inner

sideForTile :: [Tile] -> Tile -> Side -> (Maybe Tile, [Tile])
sideForTile tiles tile side =
  let output = headMay . catMaybes . map (tileMatchesBorder (getBorder tile side) (otherSide side)) $ tiles
      remaining = case output of
        Nothing -> tiles
        Just t -> filter (not . (==) (tileId t) . tileId) tiles
  in (output, remaining)

findTopLeft :: [Tile] -> Maybe (Tile, [Tile])
findTopLeft tiles = headMay . catMaybes . flip map tiles $ \ tile ->
  let remaining = filter (not . (==) (tileId tile) . tileId) tiles
      isTopLeft tile = and
        [ isJust . fst . sideForTile remaining tile $ SideBottom
        , isJust . fst . sideForTile remaining tile $ SideRight
        , isNothing . fst . sideForTile remaining tile $ SideTop
        , isNothing . fst . sideForTile remaining tile $ SideLeft
        ]
  in if isTopLeft tile then Just (tile, remaining) else Nothing

iterateRow :: Tile -> Side -> [Tile] -> ([Tile], [Tile])
iterateRow tile side tiles = case sideForTile tiles tile side of
  (Nothing, _) -> ([tile], tiles)
  (Just this, remaining) ->
    let (right, newRemaining) = iterateRow this side remaining
    in (tile:right, newRemaining)

iterateImage :: Tile -> [Tile] -> IO [[Tile]]
iterateImage tile = \ case
  [] -> pure [[tile]]
  remaining -> do
    let (top, afterTop) = iterateRow tile SideRight remaining
        (left, afterLeft) = iterateRow tile SideBottom afterTop
    inner <- case headMay $ drop 1 top of
      Nothing -> pure []
      Just oneRight -> case sideForTile afterLeft oneRight SideBottom of
        (Nothing, _) -> fail "Couldn't find inner top left"
        (Just innerTopLeft, newRemaining) -> iterateImage innerTopLeft newRemaining
    pure $ top:(zipWith (:) (drop 1 left) inner)

removeImageBorders :: [[Tile]] -> [[Char]]
removeImageBorders = foldr (\ next acc -> removeTileRowBorder next <> acc) mempty
  where
    removeTileRowBorder tiles = foldr (\ next acc -> zipWith (<>) (removeTileBorder next) acc) (replicate (maybe 0 (length . tileData) $ headMay tiles) []) tiles
    removeTileBorder = map (drop 1 . dropEnd 1) . drop 1 . dropEnd 1 . tileData

seaMonsterHeight, seaMonsterWidth, numSeaMonsterChars :: Int
seaMonsterHeight = 3
seaMonsterWidth = 20
numSeaMonsterChars = length $ filter ((==) '#') seaMonster

seaMonster :: [Char]
seaMonster = mconcat
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

overlaysSeaMonster :: [[Char]] -> (Int, Int) -> Bool
overlaysSeaMonster under (x, y) =
  all (\ (i, j) -> if i == ' ' then True else i == j) . zip seaMonster . concatMap (take seaMonsterWidth . drop y) . take seaMonsterHeight . drop x $ under

main :: IO ()
main = do
  tiles <- either fail pure . Atto.parseOnly tilesParser =<< readFile "day20.txt"
  (topLeft, remaining) <- maybe (fail "Couldn't find top left") pure $ findTopLeft tiles
  image <- removeImageBorders <$> iterateImage topLeft remaining
  let imageLen = length image - 1
      numPoundChars = length . filter ((==) '#') . mconcat $ image
      coords = [ (x, y) | x <- [0..(imageLen - seaMonsterHeight)], y <- [0..(imageLen - seaMonsterWidth)] ]
  for_ (getChoices image) $ \ choice -> do
    let numChars = (*) numSeaMonsterChars . length . filter (overlaysSeaMonster choice) $ coords
    if numChars == 0 then pure () else putStrLn $ tshow $ numPoundChars - numChars
