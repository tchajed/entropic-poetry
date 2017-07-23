{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module FormatParserSpec where

import Parser
import Syntax

import Control.Monad.Identity (Identity)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec
import Text.Parsec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

{-# ANN module ("HLint: ignore Use let" :: String) #-}

shouldParseWithNames
  :: (Show a, Eq a)
  => [String]
  -> (ParsecT T.Text (Set.Set Name) Identity a, T.Text)
  -> a
  -> Expectation
shouldParseWithNames names (p, s) x =
  let boundNames = Set.fromList (map Name names)
  in runParser p boundNames "" s `shouldBe` Right x

shouldParseTo
  :: (Show a, Eq a)
  => (ParsecT T.Text (Set.Set Name) Identity a, T.Text) -> a -> Expectation
shouldParseTo = shouldParseWithNames []

shouldNotParse
  :: (Show a, Eq a)
  => ParserT a -> String -> Expectation
shouldNotParse p s =
  runParser p Set.empty "" s `shouldSatisfy` \r ->
    case r of
      Left _ -> True
      Right _ -> False

spec :: Spec
spec = do
  describe "conjugation" $ do
    it "past conjugation should parse" $ do
      (conjugation, "past") `shouldParseTo` Past
    it "participle conjugation should parse" $ do
      (conjugation, "participle") `shouldParseTo` Participle
    it "invalid conjugation should not parse" $ do
      conjugation `shouldNotParse` "foobar"
  describe "type" $ do
    it "noun type should parse" $ do (typeP, "noun") `shouldParseTo` Noun
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
    it "unbound reference type should not parse" $ do
      typeP `shouldNotParse` "?foobar"
    it "bound reference type should parse" $ do
      shouldParseWithNames
        ["foobar"]
        (typeP, "?foobar")
        (Reference (Name "foobar"))
  describe "identifier" $ do
    it "should parse alphabetic identifiers" $ do
      (identifier, "foo") `shouldParseTo` Name "foo"
    it "should parse alpha followed by digits" $ do
      (identifier, "a1") `shouldParseTo` Name "a1"
    it "should allow _" $ do
      (identifier, "a_binding_name") `shouldParseTo` Name "a_binding_name"
    it "should not allow empty" $ do identifier `shouldNotParse` ""
    it "should not allow initial digit" $ do
      identifier `shouldNotParse` "123name"
  describe "placeholder" $ do
    it "should parse unbound noun" $ do
      (placeholder, "{noun}") `shouldParseTo` PlainType Noun
    it "should parse bound noun" $ do
      (placeholder, "{name:noun}") `shouldParseTo` Binding (Name "name") Noun
    it "should parse bound verb" $ do
      (placeholder, "{a1:verb(past)}") `shouldParseTo`
        Binding (Name "a1") (Verb Past)
    it "preposition function call should not parse" $ do
      typeP `shouldNotParse` "{preposition()}"
    it "should not parse unbound references" $ do
      placeholder `shouldNotParse` "{?name}"
    it "should parse references" $ do
      shouldParseWithNames
        ["name"]
        (placeholder, "{?name}")
        (PlainType (Reference (Name "name")))
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
      shouldParseWithNames
        ["name"]
        (placeholder, "{name2:?name}")
        (Binding (Name "name2") (Reference (Name "name")))
  describe "format" $ do
    noun <- return $ Placeholder (PlainType Noun)
    it "should parse just literal" $ do
      (format, "foobar") `shouldParseTo` [Literal "foobar"]
    it "should parse literal and placeholder" $ do
      (format, "foobar {noun}") `shouldParseTo` [Literal "foobar ", noun]
    it "should parse placeholders and literals" $ do
      (format, "{noun} foobar {noun} text") `shouldParseTo`
        [noun, Literal " foobar ", noun, Literal " text"]
    it "should ignore comments" $ do
      (format, "# comment\nfoobar {noun}") `shouldParseTo`
                -- TODO: the comment should really have the newline
        [Comment " comment", Literal "foobar ", noun]
    it "should preserve newlines" $ do
      (format, "some\ntext{noun}\nhere") `shouldParseTo`
        [Literal "some\ntext", noun, Literal "\nhere"]
    it "should consume a single newline after a comment" $ do
      (format, "# comment\n\nfoobar {noun}") `shouldParseTo`
        [Comment " comment", Literal "\nfoobar ", noun]
    it "should parse adjacent placeholders" $ do
      (format, "{noun}{verb(past)}") `shouldParseTo`
        [noun, Placeholder (PlainType (Verb Past))]
    it "should parse bound references" $ do
      (format, "{name:noun} {?name}") `shouldParseTo`
        [ Placeholder (Binding (Name "name") Noun)
        , Literal " "
        , Placeholder (PlainType (Reference (Name "name")))
        ]
    it "should parse re-bound references references" $ do
      (format, "{name:noun}{name2:?name}{?name2}") `shouldParseTo`
        (let name = Name "name"
             name2 = Name "name2"
         in [ Placeholder (Binding name Noun)
            , Placeholder (Binding name2 (Reference name))
            , Placeholder (PlainType (Reference name2))
            ])
