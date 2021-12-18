#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative ((<|>))

data Snail = Regular Int | Pair Snail Snail
  deriving (Eq, Ord, Show)

doLeftmost :: (Int -> Int) -> Snail -> Snail
doLeftmost f = \case
  Regular i -> Regular (f i)
  Pair l r -> Pair (doLeftmost f l) r

doRightmost :: (Int -> Int) -> Snail -> Snail
doRightmost f = \case
  Regular i -> Regular (f i)
  Pair l r -> Pair l (doRightmost f r)

explodeSnail :: Snail -> Maybe Snail
explodeSnail snail =
  let inner depth = \case
        p@(Pair (Regular l) (Regular r)) | depth == 4 -> Just (Regular 0, (doRightmost ((+) l), doLeftmost ((+) r)))
        p@(Pair l r) -> case (inner (depth + 1) l, inner (depth + 1) r) of
          (Just (newL, (f, g)), _) -> Just (Pair newL (g r), (f, id))
          (_, Just (newR, (f, g))) -> Just (Pair (f l) newR, (id, g))
          _ -> Nothing
        _ -> Nothing
  in fst <$> inner 0 snail

splitSnail :: Snail -> Maybe Snail
splitSnail = \case
  Regular i -> if i >= 10 then Just (Pair (Regular (i `div` 2)) (Regular ((i `div` 2) + (i `mod` 2)))) else Nothing
  Pair l r -> (flip Pair r <$> splitSnail l) <|> (Pair l <$> splitSnail r)

reduceSnail :: Snail -> Snail
reduceSnail snail = maybe snail reduceSnail $ explodeSnail snail <|> splitSnail snail

snailMagnitude :: Snail -> Int
snailMagnitude = \case
  Pair l r -> 3 * (snailMagnitude l) + 2 * (snailMagnitude r)
  Regular i -> i

parseSnail :: [Char] -> (Snail, [Char])
parseSnail = \case
  '[':xs ->
    let (l, ',':ys) = parseSnail xs
        (r, ']':zs) = parseSnail ys
    in (Pair l r, zs)
  x:xs -> (Regular $ read [x], xs)

getFileContents :: FilePath -> IO [Snail]
getFileContents fp = do
  fmap (fst . parseSnail) . lines <$> readFile fp

main :: IO ()
main = do
  allSnails@(snail:snails) <- getFileContents "day18.txt"
  let reduced = snailMagnitude $ foldl (\acc next -> reduceSnail (Pair acc next)) snail snails
      largest = maximum . fmap (snailMagnitude . reduceSnail . uncurry Pair) . filter (uncurry (/=)) $ [ (x, y) | x <- allSnails, y <- allSnails ]
  putStrLn $ show reduced
  putStrLn $ show largest
