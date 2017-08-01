{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Parser where

import Control.Monad (void)
import qualified Data.Set as Set
import Data.Text (Text)
import Syntax
import Text.Parsec

type DefinedBindings = Set.Set Name

type ParserT a = forall m s. Stream s m Char =>
                               ParsecT s DefinedBindings m a

conjugation :: ParserT Conjugation
conjugation =
  choice
    [ try (string "past") >> return Past
    , try (string "participle") >> return Participle
    , try (string "infinitive") >> return Infinitive
    ] <?>
  "conjugation"

identifier :: ParserT Name
identifier =
  Name <$> do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    return (c : cs)

functionCall :: String -> ParserT a -> ParserT a
functionCall name p = string name >> between (char '(') (char ')') p

validBinding :: Name -> ParserT Bool
validBinding n = Set.member n <$> getState

typeP :: ParserT Type
typeP =
  choice
    [ string "noun" >> return Noun
    , Verb <$> functionCall "verb" conjugation
    , string "preposition" >> return Preposition
    , do _ <- char '?'
         n <- identifier
         valid <- validBinding n
         if valid
           then return $ Reference n
           else let Name name = n
                in unexpected $ "invalid reference " ++ name
    , OneOf <$> functionCall "oneof" (literal `sepBy` char ',')
    ]
  where
    literal :: ParserT String
    literal = many (noneOf ",)")

placeholder :: ParserT Placeholder
placeholder =
  between (char '{') (char '}') (try binderPlaceholder <|> typePlaceholder)
  where
    binderPlaceholder = do
      n <- identifier
      _ <- char ':'
      t <- typeP
      modifyState (Set.insert n)
      return $ Binding n t
    typePlaceholder = PlainType <$> typeP

-- TODO: comments should also be terminated by eof
comment :: ParserT String
comment = char '#' >> manyTill anyChar (try (char '\n'))

tokenP :: ParserT Token
tokenP =
  choice
    [ Comment <$> comment
    , Placeholder <$> placeholder
    -- TODO: surely there's a simpler way to stop just before a placeholder
    -- (without resorting to looking ahead for a char '{')
    , Literal <$> manyTill anyChar (void (try (lookAhead placeholder)) <|> eof)
    ]

format :: ParserT Format
format = manyTill tokenP eof

-- String is used as a filename
parseFormat :: String -> Text -> Either ParseError Format
parseFormat = runParser format Set.empty
