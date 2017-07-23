{-# LANGUAGE OverloadedStrings #-}

module WordListParserSpec where

import Syntax
import WordList

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Text.Parsec (runParser)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

{-# ANN module ("HLint: ignore Use let" :: String) #-}

shouldParseTo :: T.Text -> [Section] -> Expectation
shouldParseTo s x = runParser wordList () "" s `shouldBe` Right x

shouldNotParse :: T.Text -> Expectation
shouldNotParse s =
  parseWordList "" s `shouldSatisfy` \r ->
    case r of
      Left _ -> True
      Right _ -> False

spec :: Spec
spec = do
  it "parses a single section" $ do
    "#verb(past)\nwent\nsat" `shouldParseTo`
      [Section (Verb Past) ["went", "sat"]]
  it "parses removing whitespace" $ do
    "# verb(past) \nwent \nsat\n" `shouldParseTo`
      [Section (Verb Past) ["went", "sat"]]
  it "removes empty lines" $ do
    "# verb(past) \nwent \nsat\n\n" `shouldParseTo`
      [Section (Verb Past) ["went", "sat"]]
  it "rejects invalid types" $ do shouldNotParse "# foo\n"
  it "parses multiple sections" $ do
    "# verb(past)\n went\n sat\n #noun\n bat\n ball" `shouldParseTo`
      [Section (Verb Past) ["went", "sat"], Section Noun ["bat", "ball"]]
