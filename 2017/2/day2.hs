import ClassyPrelude
import qualified Data.ByteString.Char8 as BS

doChecksum :: FilePath -> IO Int
doChecksum path = do
  input <- BS.unpack <$> readFile path
  is <- forM (words <$> lines input) $ \ xs -> forM xs $ \ x ->
    maybe (fail $ "not an int: " <> x) pure $ readMay x
  pure $ checksum is

checksumrow :: [Int] -> Int
checksumrow xs =
  let rowMin = maybe 0 minimum $ fromNullable xs
      rowMax = maybe 0 maximum $ fromNullable xs
  in rowMax - rowMin

checksum :: [[Int]] -> Int
checksum = sum . map checksumrow
