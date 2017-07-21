module Syntax where

newtype Name = Name String
    deriving (Eq, Show, Ord)

data Conjugation =
    Past
    | Participle
    | Infinitive
    deriving (Eq, Show, Ord)

data Type =
    Verb Conjugation
    | Noun
    | Preposition
    -- these types are special: they do not need backing data
    | Reference Name
    | OneOf [String]
    deriving (Eq, Show, Ord)

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
