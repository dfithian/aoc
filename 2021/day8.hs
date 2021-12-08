#!/usr/bin/env stack
-- stack --resolver lts script

import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

data Ordinal = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Ord, Show, Enum, Bounded)

data Signal = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show)

toInt :: [Ordinal] -> Int
toInt = sum . zipWith (*) [1000, 100, 10, 1] . fmap fromEnum

zero, one, two, three, four, five, six, seven, eight, nine :: Set Signal
zero = Set.fromList [A, B, C, E, F, G]
one = Set.fromList [C, F]
two = Set.fromList [A, C, D, E, G]
three = Set.fromList [A, C, D, F, G]
four = Set.fromList [B, C, D, F]
five = Set.fromList [A, B, D, F, G]
six = Set.fromList [A, B, D, E, F, G]
seven = Set.fromList [A, C, F]
eight = Set.fromList [A, B, C, D, E, F, G]
nine = Set.fromList [A, B, C, D, F, G]

ordinals :: Map (Set Signal) Ordinal
ordinals = Map.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] [minBound..maxBound]

signalCounts :: Map Int Int
signalCounts = Map.fromList [(2, 1), (3, 1), (4, 1), (7, 1), (6, 2), (5, 3)]

getSignalRefs :: [String] -> Map Char Signal
getSignalRefs = Map.map (fromRight (error "impossible!")) . foldl (flip go) mempty . sortOn (\str -> let len = length str in (fromJust $ Map.lookup len signalCounts, len))
  where
    fillMissing ps xs acc = foldr (\c -> Map.insert c ps) acc $ filter (\x -> isNothing $ Map.lookup x acc) xs

    go next acc = case length next of
      2 -> fillMissing (Left (Set.fromList [C, F])) next acc -- this is a 1
      3 -> fillMissing (Right A) next acc -- this is a 7
      4 -> fillMissing (Left (Set.fromList [B, D])) next acc -- this is a 4
      7 -> fillMissing (Left (Set.fromList [E, G])) next acc -- this is a 8

      -- this is a 0, 6, or 9
      6 ->
        -- if it's a 6, then it has F and we can find C
        -- if it's a 9, then it has G and we can find E
        -- if it's a 0, then it has B and we can find D

        let nextSet = Set.fromList next
            [x1] = Set.toList (Map.keysSet acc \\ Set.fromList next)
            ys = Map.keysSet $ Map.filter ((==) (Left (Set.fromList [C, F]))) acc
            zs = Map.keysSet $ Map.filter ((==) (Left (Set.fromList [E, G]))) acc
            ws = Map.keysSet $ Map.filter ((==) (Left (Set.fromList [B, D]))) acc

        in case (Set.size (ys \\ nextSet), Set.size (zs \\ nextSet), Set.size (ws \\ nextSet)) of
          -- this is a 6, and x1 is C
          (1, _, _) ->
            let [y1, y2] = Set.toList ys
                f = if x1 == y1 then y2 else y1
            in Map.insert x1 (Right C) $ Map.insert f (Right F) acc

          -- this is a 9, and x1 is E
          (_, 1, _) ->
            let [z1, z2] = Set.toList zs
                g = if x1 == z1 then z2 else z1
            in Map.insert x1 (Right E) $ Map.insert g (Right G) acc

          -- this is a 0, and x1 is D
          (_, _, 1) ->
            let [w1, w2] = Set.toList ws
                b = if x1 == w1 then w2 else w1
            in Map.insert x1 (Right D) $ Map.insert b (Right B) acc

      -- this is a 2, 3, or 5, and everything has been found, so we're good
      _ -> acc

getOrdinals :: ([String], [String]) -> [Ordinal]
getOrdinals (signals, values) =
  let signalRefs = getSignalRefs signals
  in fmap (fromJust . flip Map.lookup ordinals . Set.fromList . fmap (fromJust . flip Map.lookup signalRefs)) values

getFileContents :: FilePath -> IO [([String], [String])]
getFileContents fp = do
  rawLines <- lines <$> readFile fp
  pure $ fmap (fmap (drop 1) . span ((/=) "|") . words) rawLines

main :: IO ()
main = do
  signalsAndValues <- getFileContents "day8.txt"
  let allOrdinals = getOrdinals <$> signalsAndValues
      uniqueOrdinalCount = length . filter (flip elem [One, Four, Seven, Eight]) . mconcat $ allOrdinals
      score = sum $ toInt <$> allOrdinals
  putStrLn $ show uniqueOrdinalCount
  putStrLn $ show score
