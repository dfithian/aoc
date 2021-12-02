#!/usr/bin/env stack
-- stack --resolver lts script

import Control.Applicative ((<|>), many)
import Control.Monad (fail, void)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

data Direction
  = Forward
  | Down
  | Up
  deriving (Eq, Show)

data Command = Command Direction Int
  deriving (Eq, Show)

type Parser = Mega.Parsec String String

direction :: Parser Direction
direction = do
  (Forward <$ Mega.string "forward")
    <|> (Down <$ Mega.string "down")
    <|> (Up <$ Mega.string "up")

command :: Parser Command
command = do
  dir <- direction
  Mega.hspace
  dist <- read <$> many Mega.digitChar
  void Mega.eol
  pure $ Command dir dist

parseCommands :: FilePath -> IO [Command]
parseCommands fp = do
  content <- readFile fp
  either (fail . show) pure $ Mega.parse (many command) fp content

main :: IO ()
main = do
  xs <- parseCommands "day2.txt"
  let go (prevX, prevZ, prevAim) (Command dir dist) = case dir of
        Forward -> (prevX + dist, prevZ + dist * prevAim, prevAim)
        Down -> (prevX, prevZ, prevAim + dist)
        Up -> (prevX, prevZ, prevAim - dist)
      (x, z, aim) = foldl go (0, 0, 0) xs
  putStrLn $ "Position " <> show x <> ", depth " <> show z <> ", aim " <> show aim <> ", total " <> show (x * z)
