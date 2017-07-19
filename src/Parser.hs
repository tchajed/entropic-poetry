{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Parser where

import Text.Parsec
import Data.Text (Text)
import Control.Monad.Identity (Identity)
import Control.Monad (void)
import Syntax

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
            (literal `sepBy` char ',')
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

-- TODO: comments should also be terminated by eof
comment :: ParserT String
comment = char '#' >> manyTill anyChar (try (char '\n'))

tokenP :: ParserT Token
tokenP = choice [
    Comment <$> comment
    , Placeholder <$> placeholder
    -- TODO: surely there's a simpler way to stop just before a placeholder
    -- (without resorting to looking ahead for a char '{')
    , Literal <$> manyTill anyChar (void (try (lookAhead placeholder)) <|> eof)
    ]

document :: ParserT Document
document = manyTill tokenP eof

-- String is used as a filename
parseFormat :: String -> Text -> Either ParseError Document
parseFormat = runParser document ()
