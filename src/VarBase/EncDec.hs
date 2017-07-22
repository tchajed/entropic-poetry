module VarBase.EncDec (
      Card
    , Word8
    , decodeNum
    , encodeNum
    , decode
    , encode
    , cardEntropy
    , numBytes
) where

import Data.Word (Word8, Word)

-- encoding: data -> variable-base number
-- decoding: variable-base -> data

type Card = Word

-- if not given enough cardinalities, returns leftover words
decodeNum :: [Card] -> [Int] -> (Integer, [Int])
decodeNum cs [] = (0, [])
decodeNum (c:cs) (w:ws) =
    let (wsVal, rest) = decodeNum cs ws in
    (fromIntegral w + fromIntegral c * wsVal, rest)
decodeNum [] ws = (0, ws)

-- returns leftover value that could not be encoded (0 if everything is encoded)
encodeNum :: [Card] -> Integer -> ([Int], Integer)
encodeNum [] n = ([], n)
encodeNum cs 0 = (replicate (length cs) 0, 0)
encodeNum (c:cs) n =
    let w = fromIntegral (n `mod` fromIntegral c) in
    let (ws, left) = encodeNum cs (n `div` fromIntegral c) in
    (w:ws, left)

cardEntropy :: Card -> Double
cardEntropy c = logBase 256 (fromIntegral c)

numBytes :: [Card] -> Int
numBytes = round . sum . map cardEntropy

-- use 256 for the cardinality of all the output bytes
byteCards :: [Card] -> [Card]
byteCards cs = replicate (numBytes cs) 256

decode :: [Card] -> [Int] -> ([Word8], [Int])
decode cs ws =
    let (n, ws') = decodeNum cs ws
        (bytes, _) = encodeNum (byteCards cs) n in
        (map fromIntegral bytes, ws')

padTo :: Int -> [Word8] -> [Word8]
padTo len bs = if len < length bs
                then replicate (length bs - len) 0 ++ bs
                else bs

encode :: [Card] -> [Word8] -> ([Int], [Word8])
encode cs bytes =
    let bs = padTo (numBytes cs) bytes
        -- can't run out of cardinalities
        (n, _) = decodeNum (byteCards cs) (map fromIntegral bs)
        (ws, n') = encodeNum cs n
        -- we need to encode the leftover bytes
        (bs', _) = encodeNum [256,256..] n' in
        (ws, map fromIntegral bs')
