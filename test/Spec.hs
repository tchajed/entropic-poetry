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

import Text.Parsec
import Control.Monad.Identity (Identity)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Use let" :: String) #-}

shouldParseTo :: (Show a, Eq a) => (ParsecT T.Text () Identity a, T.Text) -> a -> Expectation
shouldParseTo (p, s) x = runParser p () "" s `shouldBe` Right x

shouldNotParse :: (Show a, Eq a) => ParserT a -> String -> Expectation
shouldNotParse p s = runParser p () "" s `shouldSatisfy`
    \r -> case r of
          Left _ -> True
          Right _ -> False

parserSpec :: Spec
parserSpec =
    describe "Parser" $ do
        describe "conjugation" $ do
            it "past conjugation should parse" $ do
                (conjugation, "past") `shouldParseTo` Past
            it "participle conjugation should parse" $ do
                (conjugation, "participle") `shouldParseTo` Participle
            it "invalid conjugation should not parse" $ do
                conjugation `shouldNotParse` "foobar"
        describe "type" $ do
            it "noun type should parse" $ do
                (typeP, "noun") `shouldParseTo` Noun
            it "verb type should parse" $ do
                (typeP, "verb(past)") `shouldParseTo` Verb Past
            it "verb type should parse with infinitive" $ do
                (typeP, "verb(infinitive)") `shouldParseTo` Verb Infinitive
            it "one of should parse with one value" $ do
                (typeP, "oneof(a)") `shouldParseTo` OneOf ["a"]
            it "one of should parse with two values" $ do
                (typeP, "oneof(a,b)") `shouldParseTo` OneOf ["a", "b"]
            it "one of should capture whitespace" $ do
                (typeP, "oneof(a ,b, c)") `shouldParseTo` OneOf ["a ", "b", " c"]
            it "preposition type should parse" $ do
                (typeP, "preposition") `shouldParseTo` Preposition
            it "reference type should parse" $ do
                (typeP, "?foobar") `shouldParseTo` Reference (Name "foobar")
        describe "identifier" $ do
            it "should parse alphabetic identifiers" $ do
                (identifier, "foo") `shouldParseTo` Name "foo"
            it "should parse alpha followed by digits" $ do
                (identifier, "a1") `shouldParseTo` Name "a1"
            it "should allow _" $ do
                (identifier, "a_binding_name") `shouldParseTo` Name "a_binding_name"
            it "should not allow empty" $ do
                identifier `shouldNotParse` ""
            it "should not allow initial digit" $ do
                identifier `shouldNotParse` "123name"
        describe "placeholder" $ do
            it "should parse unbound noun" $ do
                (placeholder, "{noun}") `shouldParseTo`
                    PlainType Noun
            it "should parse bound noun" $ do
                (placeholder, "{name:noun}") `shouldParseTo`
                    Binding (Name "name") Noun
            it "should parse bound verb" $ do
                (placeholder, "{a1:verb(past)}") `shouldParseTo`
                    Binding (Name "a1") (Verb Past)
            it "preposition function call should not parse" $ do
                typeP `shouldNotParse` "{preposition()}"
            it "should parse references" $ do
                (placeholder, "{?name}") `shouldParseTo` PlainType (Reference (Name "name"))
            it "should parse bound oneof" $ do
                (placeholder, "{sel:oneof(a,b,c)}") `shouldParseTo`
                    Binding (Name "sel") (OneOf ["a", "b", "c"])
            it "should parse numbers and symbols in oneof" $ do
                (placeholder, "{oneof(1,*,/)}") `shouldParseTo`
                    PlainType (OneOf ["1", "*", "/"])
            it "should parse empty string in oneof" $ do
                (placeholder, "{oneof(a,,b)}") `shouldParseTo`
                    PlainType (OneOf ["a", "", "b"])
            it "should parse bound reference" $ do
                (placeholder, "{name2:?name}") `shouldParseTo`
                    Binding (Name "name2") (Reference (Name "name"))
        describe "document" $ do
            noun <- return $ Placeholder (PlainType Noun)
            it "should parse just literal" $ do
                (document, "foobar") `shouldParseTo` [Literal "foobar"]
            it "should parse literal and placeholder" $ do
                (document, "foobar {noun}") `shouldParseTo`
                    [Literal "foobar ", noun]
            it "should parse placeholders and literals" $ do
                (document, "{noun} foobar {noun} text") `shouldParseTo`
                    [noun, Literal " foobar ", noun, Literal " text"]
            it "should ignore comments" $ do
                (document, "# comment\nfoobar {noun}") `shouldParseTo`
                    -- TODO: the comment should really have the newline
                    [Comment " comment", Literal "foobar ", noun]
            it "should preserve newlines" $ do
                (document, "some\ntext{noun}\nhere") `shouldParseTo`
                    [Literal "some\ntext", noun, Literal "\nhere"]
            it "should consume a single newline after a comment" $ do
                (document, "# comment\n\nfoobar {noun}") `shouldParseTo`
                    [Comment " comment", Literal "\nfoobar ", noun]
            it "should parse adjacent placeholders" $ do
                (document, "{noun}{verb(past)}") `shouldParseTo`
                    [noun, Placeholder (PlainType (Verb Past))]

-- |Generate a list of cardinalities.
cardGen :: Gen [Card]
cardGen = listOf1 (choose (1, 2048))

encodeDecodeNum :: [Card] -> Integer -> (Integer, Integer, [Int])
encodeDecodeNum cs n =
    let (ws, n') = VB.encodeNum cs n
        (n2, ws') = VB.decodeNum cs ws in
        (n2, n', ws')

-- |Generate a number that fits in a list of cardinalities.
cardNumGen :: [Card] -> Gen Integer
cardNumGen cs = choose (0, product (map fromIntegral cs) - 1)

decodeEncodeNum :: [Card] -> [Int] -> ([Int], [Int], Integer)
decodeEncodeNum cs ws =
    let (n, ws') = VB.decodeNum cs ws
        (ws2, n') = VB.encodeNum cs n in
        (ws2, ws', n')

encodeDecode :: [Card] -> [Word8] -> ([Word8], [Word8], [Int])
encodeDecode cs bs =
    let (ws, bs') = VB.encode cs bs
        (bs2, ws') = VB.decode cs ws in
        (bs2, bs', ws')

-- |Generate bytes that fit in a list of cardinalities.
cardBytesGen :: [Card] -> Gen [Word8]
cardBytesGen cs =
    vector (VB.numBytes cs)

decodeEncode :: [Card] -> [Int] -> ([Int], [Int], [Word8])
decodeEncode cs ws =
    let (bs, ws') = VB.decode cs ws
        (ws2, bs') = VB.encode cs bs in
        (ws2, ws', bs')

-- |Generate words corresponding to cardinalities.
cardWordsGen :: [Card] -> Gen [Int]
cardWordsGen = mapM (\c -> choose (0, fromIntegral c-1))

-- TODO: test partial input/cardinalities better
encdecSpec :: Spec
encdecSpec = do
    describe "variable-base encoder/decoder" $ do
        describe "to numbers" $ do
            it "decode >>> encode = id example" $ do
                cs <- return [3,4,6,2]
                ws <- return [2,0,3,1]
                decodeEncodeNum cs ws `shouldBe` (ws, [], 0)
            it "decode >>> encode = id prop" $ do
                forAll cardGen $ \cs ->
                    forAll (cardWordsGen cs) $ \ws ->
                        decodeEncodeNum cs ws `shouldBe` (ws, [], 0)
            it "encode >>> decode = id example" $ do
                cs <- return [3,4,6,2]
                n <- return 102
                -- sanity check on constant
                n `shouldSatisfy` (< product (map fromIntegral cs))
                encodeDecodeNum cs n `shouldBe` (n, 0, [])
            it "encode >>> decode = id prop" $ do
                forAll cardGen $ \cs ->
                    forAll (cardNumGen cs) $ \n ->
                        encodeDecodeNum cs n `shouldBe` (n, 0, [])
        describe "to bytes" $ do
            it "decode >>> encode = id" $ do
                forAll cardGen $ \cs ->
                    forAll (cardWordsGen cs) $ \ws ->
                        let (ws2, ws', _) = decodeEncode cs ws in
                            (ws2, ws') `shouldBe` (ws, [])
            it "encode >>> decode = id" $ do
                forAll cardGen $ \cs ->
                    forAll (cardBytesGen cs) $ \bs ->
                        let (bs2, _, ws') = encodeDecode cs bs in
                            (take (VB.numBytes cs) bs2, ws') `shouldBe` (bs, [])

parseWordListSpec :: Spec
parseWordListSpec = do
    describe "word list parser" $ do
        it "parses a single section" $ do
            (wordList, "#verb(past)\nwent\nsat") `shouldParseTo`
                [Section (Verb Past) ["went", "sat"]]
        it "parses removing whitespace" $ do
            (wordList, "# verb(past) \nwent \nsat\n") `shouldParseTo`
                [Section (Verb Past) ["went", "sat"]]
        it "removes empty lines" $ do
            (wordList, "# verb(past) \nwent \nsat\n\n") `shouldParseTo`
                [Section (Verb Past) ["went", "sat"]]
        it "rejects invalid types" $ do
            wordList `shouldNotParse` "# foo\n"
        it "parses multiple sections" $ do
            (wordList, "# verb(past)\n went\n sat\n #noun\n bat\n ball") `shouldParseTo`
                [ Section (Verb Past) ["went", "sat"]
                , Section Noun ["bat", "ball"]]

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

-- TODO: switch to auto-discovered tests
main :: IO ()
main = hspec $ do
    parserSpec
    encdecSpec
    parseWordListSpec
    encodePoemSpec
