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
import Control.Monad.Reader
import Control.Monad (zipWithM)
import qualified Data.ByteString as BS
import Data.List (elemIndex, stripPrefix)
import Data.Maybe (catMaybes)

newtype WordDatabase = WordDatabase
    { _dbLookup :: Type -> [String] }

type Ctx = Map.Map Name String

type DbMonad a = forall k. Monad k => ReaderT WordDatabase k a

type CtxMT m a = ReaderT WordDatabase (StateT Ctx m) a

type CtxM a = forall m. Monad m => CtxMT m a

wordsForType :: Type -> DbMonad [String]
wordsForType t = asks (`_dbLookup` t)

lookupName :: Name -> CtxM String
lookupName name@(Name n) =
    let def = "{?" ++ n ++ "}" in
    gets (Map.findWithDefault def name)

getWords :: Type -> CtxM [String]
getWords t =
    case t of
        Reference n -> do
            w <- lookupName n
            return [w]
        OneOf ws -> return ws
        _ -> wordsForType t

-- this could be length <$> getWords db t, but we can implement this without a
-- context
typeCard :: Type -> DbMonad Int
typeCard t =
    case t of
        Reference n -> return 1
        OneOf ws -> return $ length ws
        _ -> length <$> wordsForType t

addBinding :: Name -> String -> CtxM ()
addBinding n s = modify (Map.insert n s)

placeholderType :: Placeholder -> Type
placeholderType (PlainType t) = t
placeholderType (Binding _ t) = t

encodeWord :: Placeholder -> Int -> CtxM String
encodeWord ph w = do
    s <- (!! w) <$> getWords (placeholderType ph)
    case ph of
        Binding n _ -> addBinding n s >> return s
        _ -> return s

encodeToken :: Token -> Int -> CtxM String
encodeToken t w =
    case t of
        Comment _ -> return ""
        Literal s -> return s
        Placeholder ph -> encodeWord ph w

encodeWords :: Document -> [Int] -> CtxM String
encodeWords d ws = concat <$> zipWithM encodeToken d ws

cardinalities :: Document -> DbMonad [VB.Card]
cardinalities = mapM card
        where card :: Token -> DbMonad VB.Card
              card (Placeholder ph) = typeCard (placeholderType ph)
              card _ = return 1

entropyBytes :: Document -> DbMonad Double
entropyBytes d = sum <$> (map cardEntropy <$> cardinalities d)
        where cardEntropy :: Int -> Double
              cardEntropy c = logBase 256 (fromIntegral c)

runCtxM :: CtxM a -> WordDatabase -> a
runCtxM m db = evalState (runReaderT m db) Map.empty

encode :: WordDatabase -> Document -> BS.ByteString -> String
encode db fmt d =
    let (ws, d') = VB.encode (runReader (cardinalities fmt) db) (BS.unpack d) in
    runCtxM (encodeWords fmt (ws ++ [0,0..])) db

{- TODO:
   [✓] decode a type by returning a list of matching prefixes of the input
   [ ] modify the state within each backtrack point for bindings
   [ ] at the top level, we will get several parses, each a list of indices and
     the remaining input
   [ ] finish off parsing by keeping only parses that consumed entire input
   [ ] fail for empty parse list and ambiguous parses
-}

decodeType :: Type -> String -> CtxMT [] (Int, String)
decodeType t s = do
    ws <- getWords t
    let parse i w = do
            rest <- stripPrefix w s
            return (i, rest)
        possibleParses = catMaybes $ zipWith parse [0..] ws in
        -- inserting this lift was really difficult
        (lift . lift) possibleParses

-- decodeToken :: WordDatabase -> Token -> String -> EncM (Int, String)
