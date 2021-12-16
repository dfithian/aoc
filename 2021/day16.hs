#!/usr/bin/env stack
-- stack --resolver lts script

{-# LANGUAGE LambdaCase #-}

import Data.List.Split (chunksOf)

data Value
  = Literal Int
  | Operator Int [Packet]
  deriving (Eq, Ord, Show)

data Packet = Packet Int Int Value
  deriving (Eq, Ord, Show)

bitsToInt :: [Bool] -> Int
bitsToInt = sum . fmap (\(i, b) -> if b then 2 ^ i else 0) . zip [0..] . reverse

showBits :: [Bool] -> String
showBits = fmap (\b -> if b then '1' else '0')

expandBits :: Char -> [Bool]
expandBits = \case
  '0' -> [False, False, False, False]
  '1' -> [False, False, False, True]
  '2' -> [False, False, True,  False]
  '3' -> [False, False, True,  True]
  '4' -> [False, True,  False, False]
  '5' -> [False, True,  False, True]
  '6' -> [False, True,  True,  False]
  '7' -> [False, True,  True,  True]
  '8' -> [True,  False, False, False]
  '9' -> [True,  False, False, True]
  'A' -> [True,  False, True,  False]
  'B' -> [True,  False, True,  True]
  'C' -> [True,  True,  False, False]
  'D' -> [True,  True,  False, True]
  'E' -> [True,  True,  True,  False]
  'F' -> [True,  True,  True,  True]

expandHex :: [Char] -> [Bool]
expandHex = mconcat . fmap expandBits

readPacket :: [Bool] -> (Packet, [Bool])
readPacket bits =
  let (versionBits, typeIdBits, valueBits) = (take 3 bits, take 3 $ drop 3 bits, drop 6 bits)
      version = bitsToInt versionBits
      typeId = bitsToInt typeIdBits
      (value, rest) = case typeId of
        4 ->
          let (literalBits, rest) = snd . foldl (\(ended, (acc, leftover)) next -> if ended then (ended, (acc, leftover <> next)) else let x:xs = next in (not x, (acc <> xs, leftover))) (False, ([], [])) . chunksOf 5 $ valueBits
          in (Literal $ bitsToInt literalBits, rest)
        _ ->
          let lengthTypeIdBit:subpacketBits = valueBits
          in case lengthTypeIdBit of
            False ->
              let subpacketLength = bitsToInt $ take 15 subpacketBits
                  inner = \case
                    (final, []) -> (reverse final, drop (15 + subpacketLength) subpacketBits)
                    (acc, next) -> let (packet, leftover) = readPacket next in inner (packet:acc, leftover)
                  (subpackets, rest) = inner ([], take subpacketLength $ drop 15 subpacketBits)
              in (Operator 0 subpackets, rest)
            True ->
              let numSubpackets = bitsToInt $ take 11 subpacketBits
                  inner = \case
                    (final, 0, leftover) -> (reverse final, leftover)
                    (acc, n, next) -> let (packet, leftover) = readPacket next in inner (packet:acc, n - 1, leftover)
                  (subpackets, rest) = inner ([], numSubpackets, drop 11 subpacketBits)
              in (Operator 1 subpackets, rest)
  in (Packet version typeId value, rest)

countVersion :: Packet -> Int
countVersion (Packet version _ value) = case value of
  Literal _ -> version
  Operator _ subpackets -> version + sum (countVersion <$> subpackets)

runPacket :: Packet -> Int
runPacket = \case
  Packet _ 0 (Operator _ subpackets) -> sum (runPacket <$> subpackets)
  Packet _ 1 (Operator _ subpackets) -> product (runPacket <$> subpackets)
  Packet _ 2 (Operator _ subpackets) -> minimum (runPacket <$> subpackets)
  Packet _ 3 (Operator _ subpackets) -> maximum (runPacket <$> subpackets)
  Packet _ 4 (Literal n) -> n
  Packet _ 5 (Operator _ [x, y]) -> if runPacket x > runPacket y then 1 else 0
  Packet _ 6 (Operator _ [x, y]) -> if runPacket x < runPacket y then 1 else 0
  Packet _ 7 (Operator _ [x, y]) -> if runPacket x == runPacket y then 1 else 0

getFileContents :: FilePath -> IO [Char]
getFileContents fp = head . lines <$> readFile fp

main :: IO ()
main = do
  bits <- expandHex <$> getFileContents "day16.txt"
  let packet = fst $ readPacket bits
  putStrLn $ show $ countVersion packet
  putStrLn $ show $ runPacket packet
