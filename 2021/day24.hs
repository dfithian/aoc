#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.Map (Map, (!))
import Text.Read (readEither)
import qualified Data.Map as Map

import Debug.Trace

data Var = W | X | Y | Z
  deriving (Eq, Ord, Show)

data Operand
  = Literal Int
  | Variable Var
  deriving (Eq, Ord, Show)

data Instruction
  = Inp Var
  | Add Var Operand
  | Mul Var Operand
  | Div Var Operand
  | Mod Var Operand
  | Eql Var Operand
  deriving (Eq, Ord, Show)

readMay :: Read a => String -> Maybe a
readMay = either (const Nothing) Just . readEither

isInput :: Instruction -> Bool
isInput = \case
  Inp _ -> True
  _ -> False

runInstruction :: (Map Var Int, [Int]) -> Instruction -> (Map Var Int, [Int])
runInstruction (vars, inputs) instruction = case instruction of
  Inp var -> (Map.insert var (head inputs) vars, tail inputs)
  Add var operand -> go (+) var operand
  Mul var operand -> go (*) var operand
  Div var operand -> go div var operand
  Mod var operand -> go mod var operand
  Eql var operand -> go (\x y -> if x == y then 1 else 0) var operand
  where
    go f var operand =
      let x = getVar var
          y = getOperand operand
          z = f x y
      in (Map.insert var z vars, inputs)
    getVar v = Map.findWithDefault 0 v vars
    getOperand = \case
      Literal i -> i
      Variable v -> getVar v

runProgram :: [Int] -> [Instruction] -> Map Var Int
runProgram inputs = fst . foldl runInstruction (mempty, inputs)

findMax :: [Instruction] -> [Int]
findMax instructions =
  let ints = [9, 8, 7, 6, 5, 4, 3, 2, 1]
      inputss =
        [ [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14] 
        | x1 <- ints
        , x2 <- ints
        , x3 <- ints
        , x4 <- ints
        , x5 <- ints
        , x6 <- ints
        , x7 <- ints
        , x8 <- ints
        , x9 <- ints
        , x10 <- ints
        , x11 <- ints
        , x12 <- ints
        , x13 <- ints
        , x14 <- ints
        ]
  in fst . head . filter ((==) 0 . snd) . fmap (\(n, inputs) -> (if n `mod` 1000000 == 0 then trace (show inputs) else id) (inputs, runProgram inputs instructions ! Z)) . zip [1..] $ inputss

validate :: [Int] -> [Instruction] -> Bool
validate inputs instructions = (runProgram inputs instructions ! Z) == 0

getFileContents :: FilePath -> IO [Instruction]
getFileContents fp = do
  let variable = \case
        "w" -> W
        "x" -> X
        "y" -> Y
        "z" -> Z
        other -> error other
      operand s = case readEither s of
        Right i -> Literal i
        Left _ -> Variable (variable s)
      parse l = case words l of
        ["inp", x] -> Inp (variable x)
        ["add", x, y] -> Add (variable x) (operand y)
        ["mul", x, y] -> Mul (variable x) (operand y)
        ["div", x, y] -> Div (variable x) (operand y)
        ["mod", x, y] -> Mod (variable x) (operand y)
        ["eql", x, y] -> Eql (variable x) (operand y)
  fmap parse . lines <$> readFile fp

main :: IO ()
main = do
  instructions <- getFileContents "day24.txt"
  putStrLn $ show $ validate [9,4,3,9,9,8,9,8,9,4,9,9,5,9] instructions
  putStrLn $ show $ validate [2,1,1,7,6,1,2,1,6,1,1,5,1,1] instructions
