module VarBase (
      Card
    , Word8
    , decodeNum
    , encodeNum
    , decode
    , encode
    , cardEntropy
    , numBytes
    , numBytes'
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
numBytes = floor . sum . map cardEntropy

numBytes' :: [Card] -> Int
numBytes' = ceiling . sum . map cardEntropy

-- use 256 for the cardinality of all the output bytes
byteCards :: Int -> [Card]
byteCards n = replicate n 256

decode :: [Card] -> [Int] -> ([Word8], [Int])
decode cs ws =
    let (n, ws') = decodeNum cs ws
        (bytes, _) = encodeNum (byteCards $ numBytes' cs) n in
        (map fromIntegral bytes, ws')

padTo :: Int -> [Word8] -> [Word8]
padTo len bs =  bs ++ replicate (len - length bs) 0

trimTo :: Int -> [Word8] -> [Word8]
trimTo = take

encode :: [Card] -> [Word8] -> ([Int], [Word8])
encode cs bytes =
    let bs = padTo (numBytes cs) bytes
        (n, bs') = decodeNum (byteCards $ numBytes' cs) (map fromIntegral bs)
        (ws, _) = encodeNum cs n in
        (ws, map fromIntegral bs')
