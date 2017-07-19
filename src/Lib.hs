{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Lib
    (
        ParserT

        -- syntax
      , Conjugation(..)
      , Name(..)
      , Type(..)
      , Placeholder(..)

        -- parsers
      , conjugation
      , identifier
      , typeP
      , placeholder
    ) where

import Text.Parsec

type ParserT a = forall m s. Stream s m Char => ParsecT s () m a

comment :: ParserT ()
comment = char '#' >> manyTill anyChar (try (char '\n')) >> return ()

data Conjugation =
    Past
    | Participle
    | Infinitive
    deriving (Eq, Show)

data Name = Name String
    deriving (Eq, Show)

data Type =
    Verb Conjugation
    | Noun
    | Preposition
    | Reference Name
    | OneOf [String]
    deriving (Eq, Show)

data Placeholder =
    PlainType Type
    | Binding Name Type
    deriving (Eq, Show)

conjugation :: ParserT Conjugation
conjugation = choice [
      try (string "past") >> return Past
    , try (string "participle") >> return Participle
    , try (string "infinitive") >> return Infinitive
    ] <?> "conjugation"

identifier :: ParserT Name
identifier = Name <$> do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    return (c:cs)

functionCall :: String -> ParserT a -> ParserT a
functionCall name p =
    string name >>
    between (char '(') (char ')') p

typeP :: ParserT Type
typeP = choice [
        string "noun" >> return Noun
        , Verb <$> functionCall "verb" conjugation
        , string "preposition" >> return Preposition
        , char '?' >> Reference <$> identifier
        , OneOf <$> functionCall "oneof"
            (literal `sepBy` (char ','))
    ]
    where
        literal :: ParserT String
        literal = many (noneOf ",)")

placeholder :: ParserT Placeholder
placeholder = between (char '{') (char '}')
    (try binderPlaceholder <|> typePlaceholder)
    where binderPlaceholder = do
            b <- identifier
            char ':'
            t <- typeP
            return $ Binding b t
          typePlaceholder = PlainType <$> typeP
