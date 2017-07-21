{-# LANGUAGE Rank2Types #-}
module EncDec (
      encode
    , WordDatabase
    , entropyBytes
) where

import Syntax
import qualified VarBase.EncDec as VB
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad (zipWithM)
import qualified Data.ByteString as BS
import Data.List (elemIndex, stripPrefix)
import Data.Maybe (catMaybes)

newtype WordDatabase = WordDatabase
    { wordsForType :: Type -> [String] }

type Ctx = Map.Map Name String

type CtxM a = forall m. Monad m => StateT Ctx m a

type CtxMT m a = StateT Ctx m a

lookupName :: Name -> CtxM String
lookupName name@(Name n) =
    let def = "{?" ++ n ++ "}" in
    gets (Map.findWithDefault def name)

getWords :: WordDatabase -> Type -> CtxM [String]
getWords db t =
    case t of
        Reference n -> do
            w <- lookupName n
            return [w]
        OneOf ws -> return ws
        _ -> return $ wordsForType db t

-- this could be length <$> getWords db t, but we can implement this without a
-- context
typeCard :: WordDatabase -> Type -> Int
typeCard db t =
    case t of
        Reference n -> 1
        OneOf ws -> length ws
        _ -> length $ wordsForType db t

addBinding :: Name -> String -> CtxM ()
addBinding n s = modify (Map.insert n s)

placeholderType :: Placeholder -> Type
placeholderType (PlainType t) = t
placeholderType (Binding _ t) = t

encodeWord :: WordDatabase -> Placeholder -> Int -> CtxM String
encodeWord db ph w = do
    s <- (!! w) <$> getWords db (placeholderType ph)
    case ph of
        Binding n _ -> addBinding n s >> return s
        _ -> return s

encodeToken :: WordDatabase -> Token -> Int -> CtxM String
encodeToken db t w =
    case t of
        Comment _ -> return ""
        Literal s -> return s
        Placeholder ph -> encodeWord db ph w

encodeWords :: WordDatabase -> Document -> [Int] -> String
encodeWords db d ws = evalState (concat <$> wordsM) Map.empty
        where wordsM :: CtxM [String]
              wordsM = zipWithM (encodeToken db) d ws

cardinalities :: WordDatabase -> Document -> [VB.Card]
cardinalities db = map card
        where card :: Token -> VB.Card
              card (Placeholder ph) = typeCard db (placeholderType ph)
              card _ = 1

entropyBytes :: WordDatabase -> Document -> Double
entropyBytes db d = sum (map cardEntropy (cardinalities db d))
        where cardEntropy :: Int -> Double
              cardEntropy c = logBase 256 (fromIntegral c)

encode :: WordDatabase -> Document -> BS.ByteString -> String
encode db fmt d =
    let (ws, d') = VB.encode (cardinalities db fmt) (BS.unpack d) in
    encodeWords db fmt (ws ++ [0,0..])

{- TODO:
   * decode a type by returning a list of matching prefixes of the input
   * modify the state within each backtrack point for bindings
   * at the top level, we will get several parses, each a list of indices and
     the remaining input
   * finish off parsing by keeping only parses that consumed entire input
   * fail for empty parse list and ambiguous parses
-}

-- Ctx -> [(a, Ctx)]
-- is exactly StateT Ctx [] a

-- we eventually want: Ctx -> [(a, Ctx)]
-- [EncM a] = [Ctx -> (a, Ctx)]
-- EncM [a] = Ctx -> ([a], Ctx)
decodeType :: WordDatabase -> Type -> String -> CtxMT [] (Int, String)
decodeType db t s = do
    ws <- getWords db t
    let parse i w = do
            rest <- stripPrefix w s
            return (i, rest)
        possibleParses = catMaybes $ zipWith parse [0..] ws in
        -- inserting this lift was really difficult
        lift possibleParses

-- decodeToken :: WordDatabase -> Token -> String -> EncM (Int, String)
