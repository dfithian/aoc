#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

import Data.List (foldl', intercalate, reverse, sort)
import Data.List.Extra (snoc)
import "containers" Data.Map (Map, (!))
import qualified "containers" Data.Map.Strict as Map

data Prog = CdTop | CdBack | CdDir String | Ls | OutFile String Int | OutDir String
  deriving (Show)

data File = Dir (Map String File) | File Int

data Path = Path [String]

renderFile :: File -> [String]
renderFile = \case
  Dir dir -> mconcat . fmap (\(name, file) -> [ name <> ":" ] <> (("  " <>) <$> renderFile file)) . Map.toList $ dir
  File size -> [show size]

instance Show File where
  show = unlines . renderFile

instance Show Path where
  show (Path ps) = ("/" <>) . intercalate "/" $ ps

root :: Path
root = Path []

pushPath :: String -> Path -> Path
pushPath p (Path ps) = Path $ snoc ps p

popPath :: Path -> Path
popPath (Path ps) = Path . reverse . drop 1 . reverse $ ps

parse :: String -> Prog
parse x = case words x of
  ["$", "cd", dir] -> case dir of
    "/" -> CdTop
    ".." -> CdBack
    other -> CdDir other
  ["$", "ls"] -> Ls
  ["dir", dir] -> OutDir dir
  [size, file] -> OutFile file (read size)

buildFilesystem :: [Prog] -> File
buildFilesystem = snd . foldl' buildOne (root, Dir mempty)
  where
    insertFile name file (Path path) (Dir dir) = case path of
      [] -> Dir $ Map.insert name file dir
      p:ps -> let child = insertFile name file (Path ps) (dir ! p) in Dir $ Map.insert p child dir
    buildOne (path, file) = \case
      CdTop -> (root, Dir mempty)
      CdBack -> (popPath path, file)
      CdDir dir -> (pushPath dir path, file)
      Ls -> (path, file)
      OutFile name size -> (path, insertFile name (File size) path file)
      OutDir name -> (path, insertFile name (Dir mempty) path file)

filesystemSize :: File -> Map String Int
filesystemSize = fmap snd . Map.filter fst . sizeOne mempty . (root,)
  where
    sizeOne acc (path, file) = case file of
      Dir dir ->
        let childAcc = foldr (flip sizeOne) acc . fmap (\(name, child) -> (pushPath name path, child)) . Map.toList $ dir
            size = sum . fmap (\(name, _) -> snd $ childAcc ! show (pushPath name path)) . Map.toList $ dir
         in Map.insert (show path) (True, size) childAcc
      File size -> Map.insert (show path) (False, size) acc

part1 :: IO ()
part1 = do
  filesystem <- buildFilesystem . fmap parse . lines <$> readFile "day7.txt"
  putStrLn . show . sum . Map.elems . Map.filter (\s -> s > 0 && s <= 100000) . filesystemSize $ filesystem

part2 :: IO ()
part2 = do
  filesystem <- buildFilesystem . fmap parse . lines <$> readFile "day7.txt"
  let sizes = filesystemSize filesystem
      need = 30000000 - (70000000 - sizes ! "/")
  putStrLn . show . head . sort . Map.elems . Map.filter (\s -> s > need) $ sizes

main :: IO ()
main = part2
