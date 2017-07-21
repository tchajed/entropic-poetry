{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module WordDatabase.Parser (
      Section(..)
    -- maybe parseSection for testing
    , parseDatabase
    , typeWordMap
) where

import Text.Parsec
import Syntax
import qualified Data.Map.Strict as Map
import Control.Monad (void)

type ParserT a = forall m s. Stream s m Char => ParsecT s () m a

word :: ParserT String
word = between (many (oneOf " \t")) (many (oneOf " \t")) (many (noneOf " \t#\n"))

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
    , wordList :: [String] }
    deriving (Eq, Show)

section :: ParserT Section
section = do
    t <- typeHeader
    char '\n'
    words <- filter (not . null) <$> sepBy word (char '\n')
    return $ Section t words

parseDatabase :: ParserT [Section]
parseDatabase = do
    sections <- many section
    eof
    return sections

typeWordMap :: [Section] -> Map.Map Type [String]
typeWordMap ss = case ss of
    [] -> Map.empty
    Section{..}:ss ->
        let m = typeWordMap ss in
            Map.insertWith (++) secType wordList m
