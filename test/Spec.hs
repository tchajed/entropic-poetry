{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Lib
import VarBase (Card, Word8)
import qualified VarBase as VB
import WordDatabase
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck
import ParserTesting

import Text.Parsec
import Control.Monad.Identity (Identity)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Use let" :: String) #-}

testDatabase :: WordList
testDatabase = Map.fromList
    [  (Verb Past, [
        "went"
      , "sat"
      , "came"
      , "hurried"
    ])
    , (Verb Participle, [
        "going"
      , "sitting"
      , "coming"
      , "hurrying"
    ])
    , (Verb Infinitive, [
        "go"
      , "sit"
      , "come"
      , "hurry"
    ])
    , (Noun, [
        "bat"
      , "ball"
      , "tower"
      , "periscope"
    ])
    , (Preposition, [
        "in"
      , "around"
      , "into"
    ]) ]

fmtLit :: T.Text -> Document
fmtLit s = case parseFormat "(literal)" s of
    Left e -> error (show e)
    Right d -> d

testFormat1 :: Document
testFormat1 = fmtLit "The child {verb(past)} a {noun} \
\and wanted to {verb(infinitive)} {preposition} a {noun}."

testFormat2 :: Document
testFormat2 = fmtLit "{name:verb(past)} {verb(past)} {verb(participle)} \
\<separator> {name2:noun} {?name} {preposition} {?name2} {noun} {verb(infinitive)} \
\{verb(participle)} {choice:oneof(a,b,c,d)} {?choice} {noun} {noun} \
\{a:noun} {b:?a} {?b} {oneof($,%)} {oneof(#,@)}."

prop_encode_decode_fmt :: Document -> Property
prop_encode_decode_fmt fmt =
    let db = testDatabase
        numBytes = floor (entropyBytes db fmt) in
            forAll (BS.pack <$> vector numBytes) $ \bs ->
                let (s, bs') = encode db fmt bs in do
                    bs' `shouldBe` []
                    decode db fmt s `shouldBe` Right bs

encodePoemSpec :: Spec
encodePoemSpec = do
    describe "poetry encoder/decoder" $ do
        it "encode >>> decode = id 1 (simple types)" $ do
            prop_encode_decode_fmt testFormat1
        it "encode >>> decode = id 2 (references)" $ do
            prop_encode_decode_fmt testFormat2

spec :: Spec
spec = do
    encodePoemSpec
