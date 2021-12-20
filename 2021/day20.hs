#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as Map

data Coord = Coord (Int, Int) | Infinity
  deriving (Eq, Ord, Show)

getPoint :: Coord -> Maybe (Int, Int)
getPoint = \case
  Coord pt -> Just pt
  Infinity -> Nothing

toInt :: [Bool] -> Int
toInt = sum . zipWith (\power b -> if b then 2 ^ power else 0) [0..] . reverse

getNumLit :: Map Coord Bool -> Int
getNumLit = length . filter id . Map.elems

nextKeys :: Map Coord Bool -> [Coord]
nextKeys image =
  let allPoints = catMaybes . fmap getPoint . Map.keys $ image
      (minX, minY) = minimum allPoints
      (maxX, maxY) = maximum allPoints
  in [ Coord (i, j) | i <- [(minX-2)..(maxX+2)], j <- [(minY-2)..(maxY+2)] ] <> [Infinity]

nextImage :: Map Int Bool -> Map Coord Bool -> Map Coord Bool
nextImage alg image = foldr go mempty $ nextKeys image
  where
    inf = fromJust $ Map.lookup Infinity image
    go = \case
      Infinity -> Map.insert Infinity . fromJust . flip Map.lookup alg . toInt . replicate 9 $ inf
      Coord (i, j) ->
        let neighbors = [(i-1,j-1), (i-1,j), (i-1,j+1), (i,j-1), (i,j), (i,j+1), (i+1,j-1), (i+1,j), (i+1,j+1)]
            pixel = fromJust . flip Map.lookup alg . toInt . foldr (\next acc -> (Map.findWithDefault inf (Coord next) image):acc) mempty $ neighbors
        in Map.insert (Coord (i, j)) pixel

getFileContents :: FilePath -> IO (Map Int Bool, Map Coord Bool)
getFileContents fp = do
  rawAlg:_:rawImage <- lines <$> readFile fp
  let alg = mconcat $ zipWith (\i -> Map.singleton i . (==) '#') [0..] rawAlg
      image = Map.insert Infinity False . mconcat . mconcat . zipWith (\i -> zipWith (\j x -> Map.singleton (Coord (i, j)) (x == '#')) [0..]) [0..] $ rawImage
  pure (alg, image)

main :: IO ()
main = do
  (alg, image) <- getFileContents "day20.txt"
  let image2 = nextImage alg image
      image3 = nextImage alg image2
      numLit3 = getNumLit image3
      numLit50 = getNumLit $ foldr (\_ next -> nextImage alg next) image3 [3..50]
  putStrLn $ show numLit3
  putStrLn $ show numLit50
