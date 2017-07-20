module VarBase.EncDec (
      Card
    , Word8
    , decodeNum
    , encodeNum
    , decode
    , encode
) where

import Data.Word (Word8)

-- encoding: data -> variable-base number
-- decoding: variable-base -> data

type Card = Int

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
encodeNum (c:cs) n =
    let w = fromIntegral (n `mod` fromIntegral c) in
    let (ws, left) = encodeNum cs (n `div` fromIntegral c) in
    (w:ws, left)

decode :: [Card] -> [Int] -> ([Word8], [Int])
decode cs ws =
    let (n, ws') = decodeNum cs ws
        -- can't run out of cardinalities
        (bytes, _) = encodeNum [256,256..] n in
        (map fromIntegral bytes, ws')

encode :: [Card] -> [Word8] -> ([Int], [Word8])
encode cs bytes =
        -- can't run out of cardinalities
    let bs = map fromIntegral bytes
        (n, _) = decodeNum [256,256..] bs
        (ws, n') = encodeNum cs n
        -- we need to encode the leftover bytes
        (bs', _) = encodeNum [256,256..] n' in
        (ws, map fromIntegral bs')
