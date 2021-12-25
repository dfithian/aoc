#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.Bimap (Bimap)
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import qualified Data.Bimap as Bimap

import Debug.Trace

data SortedList a = SNil | SCons a (SortedList a)
  deriving (Eq, Ord, Show)

insertS :: Ord a => a -> SortedList a -> SortedList a
insertS x = \case
  SNil -> SCons x SNil
  SCons y ys -> case x < y of
    True -> SCons x (SCons y ys)
    False -> SCons y (insertS x ys)

unconsS :: SortedList a -> Maybe (a, SortedList a)
unconsS = \case
  SNil -> Nothing
  SCons x xs -> Just (x, xs)

headMayS :: SortedList a -> Maybe a
headMayS = \case
  SNil -> Nothing
  SCons x _ -> Just x

fromListS :: Ord a => [a] -> SortedList a
fromListS = \case
  [] -> SNil
  x:xs -> SCons x (fromListS xs)

takeWhileS :: (a -> Bool) -> SortedList a -> SortedList a
takeWhileS f = \case
  SNil -> SNil
  SCons x xs -> case f x of
    True -> SCons x (takeWhileS f xs)
    False -> SNil

lengthS :: SortedList a -> Int
lengthS = \case
  SNil -> 0
  SCons _ xs -> 1 + lengthS xs

instance Functor SortedList where
  fmap f = \case
    SNil -> SNil
    SCons x xs -> SCons (f x) (fmap f xs)

instance Semigroup (SortedList a) where
  SNil <> ys = ys
  SCons x xs <> ys = SCons x (xs <> ys)

instance Monoid (SortedList a) where
  mempty = SNil
  mappend = (<>)

data Amphipod = Amber1 | Amber2 | Amber3 | Amber4 | Bronze1 | Bronze2 | Bronze3 | Bronze4 | Copper1 | Copper2 | Copper3 | Copper4 | Desert1 | Desert2 | Desert3 | Desert4
  deriving (Eq, Ord, Show)

data Pos = P1 | P2 | P3 | P4
  deriving (Eq, Ord, Show)

data Location = H1 | H2 | H3 | R1 Pos | H4 | H5 | R2 Pos | H6 | H7 | R3 Pos | H8 | H9 | R4 Pos | H10 | H11
  deriving (Eq, Ord, Show)

data Cost = Cost Int | Infinite
  deriving (Eq, Show)

instance Ord Cost where
  compare (Cost x) (Cost y) = compare x y
  compare (Cost _) Infinite = LT
  compare Infinite (Cost _) = GT
  compare Infinite Infinite = EQ

instance Num Cost where
  (Cost x) + (Cost y) = Cost $ x + y
  _ + _ = Infinite

  (Cost x) * (Cost y) = Cost $ x * y
  _ * _ = Infinite

  abs (Cost x) = Cost (abs x)
  abs Infinite = Infinite

  signum (Cost x) = Cost (signum x)
  signum Infinite = Infinite

  fromInteger = Cost . fromInteger

  negate (Cost x) = Cost (negate x)
  negate Infinite = Infinite

data Board = Board (Bimap Amphipod Location, Bimap Amphipod Location, Cost)

instance Eq Board where
  (Board (_, _, x)) == (Board (_, _, y)) = x == y

instance Ord Board where
  compare (Board (_, _, x)) (Board (_, _, y)) = compare x y

nextHall :: Location -> Location
nextHall = \case
  H1 -> H2
  H2 -> H3
  H3 -> H4
  H4 -> H5
  H5 -> H6
  H6 -> H7
  H7 -> H8
  H8 -> H9
  H9 -> H10
  H10 -> H11

prevHall :: Location -> Location
prevHall = \case
  H11 -> H10
  H10 -> H9
  H9 -> H8
  H8 -> H7
  H7 -> H6
  H6 -> H5
  H5 -> H4
  H4 -> H3
  H3 -> H2
  H2 -> H1

nextPos :: Pos -> Pos
nextPos = \case
  P1 -> P2
  P2 -> P3
  P3 -> P4

prevPos :: Pos -> Pos
prevPos = \case
  P4 -> P3
  P3 -> P2
  P2 -> P1

isRoom :: Location -> Bool
isRoom = \case
  R1 _ -> True
  R2 _ -> True
  R3 _ -> True
  R4 _ -> True
  _ -> False

between :: Location -> Location -> [Location]
between l1 l2 =
  let newLocation = case (l1 == l2, l1, l2) of
        (True, _, _) -> Nothing
        (_, R1 p1, R1 p2) -> if p1 < p2 then Just $ R1 $ nextPos p1 else Just $ R1 $ prevPos p1
        (_, R2 p1, R2 p2) -> if p1 < p2 then Just $ R2 $ nextPos p1 else Just $ R2 $ prevPos p1
        (_, R3 p1, R3 p2) -> if p1 < p2 then Just $ R3 $ nextPos p1 else Just $ R3 $ prevPos p1
        (_, R4 p1, R4 p2) -> if p1 < p2 then Just $ R4 $ nextPos p1 else Just $ R4 $ prevPos p1
        (_, R1 P1, _) -> Just H3
        (_, R1 p, _) -> Just $ R1 $ prevPos p
        (_, R2 P1, _) -> Just H5
        (_, R2 p, _) -> Just $ R2 $ prevPos p
        (_, R3 P1, _) -> Just H7
        (_, R3 p, _) -> Just $ R3 $ prevPos p
        (_, R4 P1, _) -> Just H9
        (_, R4 p, _) -> Just $ R4 $ prevPos p
        (_, H3, R1 _) -> Just $ R1 P1
        (_, H5, R2 _) -> Just $ R2 P1
        (_, H7, R3 _) -> Just $ R3 P1
        (_, H9, R4 _) -> Just $ R4 P1
        _ -> if l1 < l2 then Just $ nextHall l1 else Just $ prevHall l1
  in maybe [] (\new -> new:(between new l2)) newLocation

moveCost :: Int -> Amphipod -> Cost
moveCost n = \case
  Amber1  -> Cost n
  Amber2  -> Cost n
  Amber3  -> Cost n
  Amber4  -> Cost n
  Bronze1 -> Cost $ 10 * n
  Bronze2 -> Cost $ 10 * n
  Bronze3 -> Cost $ 10 * n
  Bronze4 -> Cost $ 10 * n
  Copper1 -> Cost $ 100 * n
  Copper2 -> Cost $ 100 * n
  Copper3 -> Cost $ 100 * n
  Copper4 -> Cost $ 100 * n
  Desert1 -> Cost $ 1000 * n
  Desert2 -> Cost $ 1000 * n
  Desert3 -> Cost $ 1000 * n
  Desert4 -> Cost $ 1000 * n

friend :: Amphipod -> [Amphipod]
friend = \case
  Amber1 -> [Amber2, Amber3, Amber4]
  Amber2 -> [Amber1, Amber3, Amber4]
  Amber3 -> [Amber1, Amber2, Amber4]
  Amber4 -> [Amber1, Amber2, Amber3]
  Bronze1 -> [Bronze2, Bronze3, Bronze4]
  Bronze2 -> [Bronze1, Bronze3, Bronze4]
  Bronze3 -> [Bronze1, Bronze2, Bronze4]
  Bronze4 -> [Bronze1, Bronze2, Bronze3]
  Copper1 -> [Copper2, Copper3, Copper4]
  Copper2 -> [Copper1, Copper3, Copper4]
  Copper3 -> [Copper1, Copper2, Copper4]
  Copper4 -> [Copper1, Copper2, Copper3]
  Desert1 -> [Desert2, Desert3, Desert4]
  Desert2 -> [Desert1, Desert3, Desert4]
  Desert3 -> [Desert1, Desert2, Desert4]
  Desert4 -> [Desert1, Desert2, Desert3]

initial :: Bimap Amphipod Location
initial = Bimap.fromList
  [ (Amber1,  R2 P4)
  , (Amber2,  R4 P1)
  , (Amber3,  R3 P3)
  , (Amber4,  R4 P2)
  , (Bronze1, R2 P1)
  , (Bronze2, R4 P4)
  , (Bronze3, R2 P3)
  , (Bronze4, R3 P2)
  , (Copper1, R1 P4)
  , (Copper2, R3 P1)
  , (Copper3, R2 P2)
  , (Copper4, R4 P3)
  , (Desert1, R1 P1)
  , (Desert2, R3 P4)
  , (Desert3, R1 P2)
  , (Desert4, R1 P3)
  ]

finalRoom :: Amphipod -> (Pos -> Location)
finalRoom = \case
  x | x `elem` [Amber1, Amber2, Amber3, Amber4] -> R1
  x | x `elem` [Bronze1, Bronze2, Bronze3, Bronze4] -> R2
  x | x `elem` [Copper1, Copper2, Copper3, Copper4] -> R3
  x | x `elem` [Desert1, Desert2, Desert3, Desert4] -> R4

finalize :: Board -> Board
finalize board@(Board (inProgress, finished, cost)) =
  let finalPosition amphipod loc =
        let room = finalRoom amphipod
            others = mapMaybe (flip Bimap.lookup finished) (friend amphipod)
        in case (loc == room P4, loc == room P3, loc == room P2, loc == room P1) of
          (True, _, _, _) -> True
          (_, True, _, _) -> room P4 `elem` others
          (_, _, True, _) -> room P4 `elem` others && room P3 `elem` others
          (_, _, _, True) -> room P4 `elem` others && room P3 `elem` others && room P2 `elem` others
          _ -> False
      (newFinished, newInProgress) = Bimap.partition finalPosition inProgress
  in Board (newInProgress, foldr (uncurry Bimap.insert) finished (Bimap.assocs newFinished), cost)

isFinished :: Board -> Bool
isFinished (Board (_, finished, _)) = Bimap.size finished == 16

boardCost :: Board -> Cost
boardCost (Board (_, _, cost)) = cost

debug :: Int -> (Board, SortedList Board, Cost) -> (Board, SortedList Board, Cost)
debug iteration out@(Board b, bs, cost) = case iteration `mod` 10000 == 0 of
  True -> trace (show (iteration, cost, lengthS bs, boardCost <$> headMayS bs)) out
  False -> out

canMoveTo :: Board -> Amphipod -> Location -> Bool
canMoveTo (Board (inProgress, finished, _)) amphipod loc =
  let current = inProgress Bimap.! amphipod
      locs = between current loc
  in and $ fmap (\loc2 -> not (Bimap.memberR loc2 inProgress) && not (Bimap.memberR loc2 finished)) locs

shouldMoveTo :: Board -> Amphipod -> Location -> [Location]
shouldMoveTo (Board (_, finished, _)) amphipod loc =
  let room = finalRoom amphipod
      numFriends = length $ mapMaybe (flip Bimap.lookup finished) (friend amphipod)
      want = case numFriends of
        0 -> room P4
        1 -> room P3
        2 -> room P2
        3 -> room P1
  in case isRoom loc of
    True -> [want, H1, H2, H4, H6, H8, H10, H11]
    False -> [want]

iterateBoard :: Cost -> Board -> SortedList Board
iterateBoard maxCost board@(Board (inProgress, finished, cost)) =
  fromListS
    . filter (\c -> boardCost c < maxCost)
    . concatMap go
    . Bimap.assocs
    $ inProgress
  where
    go (amphipod, loc) =
      fmap (\loc2 -> finalize (Board (Bimap.insert amphipod loc2 inProgress, finished, moveCost (length (between loc loc2)) amphipod + cost)))
        . filter (\loc2 -> canMoveTo board amphipod loc2 && loc /= loc2)
        $ shouldMoveTo board amphipod loc

reduce :: Board -> Cost
reduce board = flip fix ((board, SNil, Infinite), 1) $ \f (state, iteration) -> case debug iteration state of
  (b, SNil, currentMin) | isFinished b -> min (boardCost b) currentMin
  (b, SCons next nexts, currentMin) | isFinished b ->
    let newMin = boardCost b
    in case newMin < currentMin of
      True -> f ((next, takeWhileS (\c -> boardCost c < newMin) nexts, newMin), iteration + 1)
      False -> f ((next, nexts, currentMin), iteration + 1)
  (b, nexts, currentMin) ->
    let allNew = iterateBoard currentMin b <> nexts
    in case unconsS allNew of
      Nothing -> currentMin
      Just (next, new) -> f ((next, new, currentMin), iteration + 1)

main :: IO ()
main = do
  putStrLn $ show $ reduce $ finalize $ Board (initial, Bimap.empty, Cost 0)
