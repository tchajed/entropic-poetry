{-# LANGUAGE OverloadedStrings #-}
module WordListParserSpec where

import WordDatabase
import Syntax

import Test.Hspec
import Test.QuickCheck
import ParserTesting

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Use let" :: String) #-}

spec :: Spec
spec = do
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
