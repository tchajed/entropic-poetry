{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module ParserTesting where

import Test.Hspec

import Parser
import Text.Parsec
import qualified Data.Text as T
import Control.Monad.Identity (Identity)

shouldParseTo :: (Show a, Eq a) => (ParsecT T.Text () Identity a, T.Text) -> a -> Expectation
shouldParseTo (p, s) x = runParser p () "" s `shouldBe` Right x

shouldNotParse :: (Show a, Eq a) => ParserT a -> String -> Expectation
shouldNotParse p s = runParser p () "" s `shouldSatisfy`
    \r -> case r of
          Left _ -> True
          Right _ -> False