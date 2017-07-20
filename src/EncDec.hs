module EncDec (
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

-- if not given enough cardinalities, returns Nothing
decodeNum :: [Card] -> [Int] -> Maybe Integer
decodeNum cs [] = Just 0
decodeNum (c:cs) (w:ws) = do
    rest <- decodeNum cs ws
    return $ fromIntegral w + fromIntegral c * rest
decodeNum [] ws = Nothing

-- returns leftover value that could not be encoded (0 if everything is encoded)
encodeNum :: [Card] -> Integer -> ([Int], Integer)
encodeNum [] n = ([], n)
encodeNum (c:cs) n =
    let w = fromIntegral (n `mod` fromIntegral c) in
    let (ws, left) = encodeNum cs (n `div` fromIntegral c) in
    (w:ws, left)

getWords :: ([Int], Integer) -> Maybe [Int]
getWords (ws, 0) = Just ws
getWords (ws, _) = Nothing

decode :: [Card] -> [Int] -> Maybe [Word8]
decode cs ws = do
    n <- decodeNum cs ws
    bytes <- getWords (encodeNum [256,256..] n)
    return $ map fromIntegral bytes

encode :: [Card] -> [Word8] -> Maybe [Int]
encode cs bytes =
    let ws = map fromIntegral bytes in do
        n <- decodeNum [256,256..] ws
        getWords (encodeNum cs n)