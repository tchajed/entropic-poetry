{-# LANGUAGE Rank2Types, FlexibleContexts #-}
import Lib

import Test.Hspec
import Text.Parsec
import Control.Monad.Identity (Identity)

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Use let" #-}

shouldParseTo :: (Stream s Identity Char, Show a, Eq a) => (ParsecT s () Identity a, s) -> a -> Expectation
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

encdecSpec :: Spec
encdecSpec = do
    describe "variable-base encoder/decoder" $ do
        describe "to numbers" $ do
            it "encode(decode) = id" $ do
                cs <- return [3,4,6,2]
                ws <- return [2,0,3,1]
                case decodeNum cs ws of
                    Just x -> encodeNum cs x `shouldBe` (ws, 0)
                    Nothing -> expectationFailure "decoding equal length should work"
            it "decode(encode) = id" $ do
                cs <- return [3,4,6,2]
                n <- return 102
                n `shouldSatisfy` (< (fromIntegral $ product cs))
                (ws, rest) <- return $ encodeNum cs n
                rest `shouldBe` 0
                decodeNum cs ws `shouldBe` Just n

-- TODO: switch to auto-discovered tests
main :: IO ()
main = hspec $ do
    parserSpec
    encdecSpec