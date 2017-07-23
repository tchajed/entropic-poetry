{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module WordList
  ( Section(..)
  , wordList
  , ParserT
  , WordList
  , getTypeWords
  , parseWordList
  ) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Syntax
import Text.Parsec

type ParserT a = forall m s. Stream s m Char =>
                               ParsecT s () m a

word :: ParserT String
word =
  between (many (oneOf " \t")) (many (oneOf " \t")) (many (noneOf " \t#\n"))

parseType :: String -> Maybe Type
parseType "verb(past)" = Just $ Verb Past
parseType "verb(participle)" = Just $ Verb Participle
parseType "verb(Infinitive)" = Just $ Verb Infinitive
parseType "noun" = Just Noun
parseType "preposition" = Just Preposition
parseType _ = Nothing

typeHeader :: ParserT Type
typeHeader = do
  name <- char '#' >> word
  case parseType name of
    Just t -> return t
    Nothing -> unexpected ("type name: " ++ name)

data Section = Section
  { secType :: Type
  , secWordList :: [String]
  } deriving (Eq, Show)

section :: ParserT Section
section = do
  t <- typeHeader
  char '\n'
  words <- filter (not . null) <$> sepBy word (char '\n')
  return $ Section t words

wordList :: ParserT [Section]
wordList = do
  sections <- many section
  eof
  return sections

type WordList = Map.Map Type [String]

getTypeWords :: Type -> WordList -> [String]
getTypeWords t = Map.findWithDefault ["(" ++ show t ++ ")"] t

typeWordMap :: [Section] -> WordList
typeWordMap ss =
  case ss of
    [] -> Map.empty
    s:ss ->
      let m = typeWordMap ss
      in Map.insertWith (++) (secType s) (secWordList s) m

parseWordList :: String -> Text -> Either ParseError WordList
parseWordList = runParser (typeWordMap <$> wordList) ()
