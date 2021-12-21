#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map

data Player = Player1 | Player2
  deriving (Eq, Ord, Show)

data State = State Player (Int, Int) (Int, Int)
  deriving (Eq, Ord, Show)

deterministic :: Int -> Int
deterministic d = 3 * d + 3

dirac :: [Int]
dirac = sum <$> [ [x, y, z] | x <- [1, 2, 3], y <- [1, 2, 3], z <- [1, 2, 3] ]

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

addScore :: Int -> (Int, Int) -> (Int, Int)
addScore turn (score, pos) =
  let newPos = ((pos - 1 + turn) `mod` 10) + 1
  in (score + newPos, newPos)

playerWins :: ((Int, Int) -> Bool) -> (Player -> (Int, Int) -> (Int, Int) -> a) -> State -> Maybe a
playerWins check extract state@(State player p1 p2) = case player of
  Player1 -> if check p2 then Just (extract Player2 p1 p2) else Nothing
  Player2 -> if check p1 then Just (extract Player1 p2 p1) else Nothing

playTurn :: Int -> State -> State
playTurn turn (State player p1 p2) = case player of
  Player1 -> State Player2 (addScore turn p1) p2
  Player2 -> State Player1 p1 (addScore turn p2)

playDeterministic :: State -> Int
playDeterministic = go 1
  where
    go die state = fromMaybe (go (die + 3) (playTurn (deterministic die) state)) $
      playerWins ((>= 1000) . fst) (\_ (loser, _) _ -> (die - 1) * loser) state

playDirac :: State -> Int
playDirac = snd . go mempty
  where
    go results state = case Map.lookup state results of
      Just (p1, p2) -> (results, max p1 p2)
      Nothing ->
        let (innerResults, score@(p1, p2)) = foldr go2 (results, (0, 0)) dirac
        in (Map.insert state score innerResults, max p1 p2)
      where
        go2 turn (accResults, score) =
          let newState = playTurn turn state
              newResults = case playerWins ((>= 21) . fst) (\winner _ _ -> winner) newState of
                Nothing -> fst $ go accResults newState
                Just Player1 -> Map.insert newState (1, 0) accResults
                Just Player2 -> Map.insert newState (0, 1) accResults
          in (newResults, addPairs (fromJust (Map.lookup newState newResults)) score)

main :: IO ()
main = do
  -- let initial = State Player1 (0, 4) (0, 8)
  let initial = State Player1 (0, 9) (0, 3)
  putStrLn $ show $ playDeterministic initial
  putStrLn $ show $ playDirac initial
