module VarBaseSpec where

import VarBase

import Test.Hspec
import Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do") #-}

{-# ANN module ("HLint: ignore Use let") #-}

-- |Generate a list of cardinalities.
cardGen :: Gen [Card]
cardGen = listOf1 (choose (1, 2048))

encodeDecodeNum :: [Card] -> Integer -> (Integer, Integer, [Int])
encodeDecodeNum cs n =
  let (ws, n') = encodeNum cs n
      (n2, ws') = decodeNum cs ws
  in (n2, n', ws')

-- |Generate a number that fits in a list of cardinalities.
cardNumGen :: [Card] -> Gen Integer
cardNumGen cs = choose (0, product (map fromIntegral cs) - 1)

decodeEncodeNum :: [Card] -> [Int] -> ([Int], [Int], Integer)
decodeEncodeNum cs ws =
  let (n, ws') = decodeNum cs ws
      (ws2, n') = encodeNum cs n
  in (ws2, ws', n')

encodeDecode :: [Card] -> [Word8] -> ([Word8], [Word8], [Int])
encodeDecode cs bs =
  let (ws, bs') = encode cs bs
      (bs2, ws') = decode cs ws
  in (bs2, bs', ws')

-- |Generate bytes that fit in a list of cardinalities.
cardBytesGen :: [Card] -> Gen [Word8]
cardBytesGen cs = vector (numBytes cs)

decodeEncode :: [Card] -> [Int] -> ([Int], [Int], [Word8])
decodeEncode cs ws =
  let (bs, ws') = decode cs ws
      (ws2, bs') = encode cs bs
  in (ws2, ws', bs')

-- |Generate words corresponding to cardinalities.
cardWordsGen :: [Card] -> Gen [Int]
cardWordsGen = mapM (\c -> choose (0, fromIntegral c - 1))

-- TODO: test partial input/cardinalities better
spec :: Spec
spec = do
  describe "encoding/decoding to numbers" $ do
    it "decode >>> encode = id" $ do
      forAll cardGen $ \cs ->
        forAll (cardWordsGen cs) $ \ws ->
          decodeEncodeNum cs ws `shouldBe` (ws, [], 0)
    it "encode >>> decode = id" $ do
      forAll cardGen $ \cs ->
        forAll (cardNumGen cs) $ \n ->
          encodeDecodeNum cs n `shouldBe` (n, 0, [])
  describe "encoding/decoding to bytes" $ do
    it "decode >>> encode = id" $ do
      forAll cardGen $ \cs ->
        forAll (cardWordsGen cs) $ \ws ->
          let (ws2, ws', _) = decodeEncode cs ws
          in (ws2, ws') `shouldBe` (ws, [])
    it "encode >>> decode = id" $ do
      forAll cardGen $ \cs ->
        forAll (cardBytesGen cs) $ \bs ->
          let (bs2, _, ws') = encodeDecode cs bs
          in (take (numBytes cs) bs2, ws') `shouldBe` (bs, [])
