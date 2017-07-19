module Syntax where

newtype Name = Name String
    deriving (Eq, Show)

data Conjugation =
    Past
    | Participle
    | Infinitive
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
    Comment String
    | Literal String
    | Placeholder Placeholder
    deriving (Eq, Show)

type Document = [Token]
