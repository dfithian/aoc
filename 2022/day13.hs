#!/usr/bin/env stack
-- stack --resolver lts-19 script

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

import Data.List (foldl', sort)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)

data Atom = ASingle Int | AList [Atom]
  deriving (Eq)

data Order = Correct | Incorrect | Cont

instance Show Atom where
  show = \case
    ASingle i -> show i
    AList is -> show is

instance Ord Atom where
  compare (ASingle a) (ASingle b) = compare a b
  compare (AList as) (AList bs) = compare as bs
  compare a (AList bs) = compare [a] bs
  compare (AList as) b = compare as [b]

parse :: String -> ([Atom], String)
parse = \case
  [] -> ([], "")
  '[':rest ->
    let (inner, rest') = parse rest
        (outer, rest'') = parse rest'
     in ((AList inner):outer, rest'')
  ']':rest -> ([], rest)
  ',':rest -> parse rest
  rest ->
    let (this, rest') = span (flip notElem ['[', ']', ',']) $ rest
     in if null this then parse rest' else let (inner, outer) = parse rest' in ((ASingle (read this)):inner, outer)

build :: String -> [([Atom], [Atom])]
build = fmap (\[a, b] -> (fst (parse (drop 1 a)), fst (parse (drop 1 b)))) . fmap (take 2) . chunksOf 3 . lines

order :: ([Atom], [Atom]) -> Order
order (as, bs) = case (as, bs) of
  ((ASingle a):as', (ASingle b):bs') -> case compare a b of
    LT -> Correct
    EQ -> order (as', bs')
    GT -> Incorrect
  ((AList a):as', (AList b):bs') -> case order (a, b) of
    Correct -> Correct
    Incorrect -> Incorrect
    Cont -> order (as', bs')
  ((ASingle a):as', _) -> order ((AList [ASingle a]):as', bs)
  (_, (ASingle b):bs') -> order (as, (AList [ASingle b]):bs')
  ([], []) -> Cont
  ([], _) -> Correct
  (_, []) -> Incorrect

score :: Int -> ([Atom], [Atom]) -> Maybe Int
score n atoms = case order atoms of
  Correct -> Just n
  Incorrect -> Nothing
  Cont -> error "Got a Cont"

two, six :: [Atom]
two = [AList [ASingle 2]]
six = [AList [ASingle 6]]

part1 :: IO ()
part1 = putStrLn . show . sum . catMaybes . zipWith score [1..] . build =<< readFile "day13.txt"

part2 :: IO ()
part2 = putStrLn . show . product . fmap fst . filter (flip elem [two, six] . snd) . zip [1..] . sort . ([two, six] <>) . concatMap (\(a, b) -> [a, b]) . build =<< readFile "day13.txt"

main :: IO ()
main = part2
