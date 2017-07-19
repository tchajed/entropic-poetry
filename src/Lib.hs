{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Lib
    (
        ParserT

        -- syntax
      , Conjugation(..)
      , Name(..)
      , Type(..)
      , Placeholder(..)
      , Token(..)
      , Document(..)

        -- parsers
      , conjugation
      , identifier
      , typeP
      , placeholder
      , document
    ) where

import Text.Parsec

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

data Token =
    Literal String
    | Placeholder Placeholder
    deriving (Eq, Show)

type Document = [Token]
data Conjugation =
    Past
    | Participle
    | Infinitive
    deriving (Eq, Show)

type ParserT a = forall m s. Stream s m Char => ParsecT s () m a

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

comment :: ParserT ()
comment = char '#' >> manyTill anyChar (try (char '\n')) >> return ()

tokenP :: ParserT Token
tokenP = choice [
    comment >> return (Literal "")
    , Placeholder <$> placeholder
    -- TODO: surely there's a simpler way to stop just before a placeholder
    -- (without resorting to looking ahead for a char '{')
    , Literal <$> manyTill anyChar ((try (lookAhead placeholder) >> return ()) <|> eof)
    ]

document :: ParserT Document
document = manyTill tokenP eof
