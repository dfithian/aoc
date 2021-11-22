import ClassyPrelude
import qualified Data.ByteString.Char8 as BS

doPassphrase :: FilePath -> IO Int
doPassphrase path = do
  input <- BS.unpack <$> readFile path
  pure . length . filter (validPassphrase . words) . lines $ input

validPassphrase :: [String] -> Bool
validPassphrase = \ case
  [] -> True
  x:xs -> notElem x xs && validPassphrase xs
