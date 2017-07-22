{-# LANGUAGE Rank2Types #-}
module EncDec (
    -- encoding
    encode

    -- decoding
    , DecodeError(..)
    , decode

    , VB.cardEntropy
    , entropyBytes
) where

import Syntax
import qualified VarBase as VB
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad (zipWithM)
import qualified Data.ByteString as BS
import Data.List (elemIndex, stripPrefix)
import Data.Maybe (catMaybes, mapMaybe)
import WordList (WordList, getTypeWords)

type Ctx = Map.Map Name String

type DbMonad a = forall k. Monad k => ReaderT WordList k a

type CtxMT m a = ReaderT WordList (StateT Ctx m) a

type CtxM a = forall m. Monad m => CtxMT m a

liftPossibilities :: [a] -> CtxMT [] a
liftPossibilities = lift . lift

wordsForType :: Type -> DbMonad [String]
wordsForType t = asks (getTypeWords t)

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

-- this could be length <$> getWords t, but we can implement this without a
-- context
typeCard :: Type -> DbMonad VB.Card
typeCard t =
    case t of
        Reference n -> return 1
        OneOf ws -> fromIntegral . length <$> return ws
        _ -> fromIntegral . length <$> wordsForType t

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

entropyBytes :: WordList -> Document -> Double
entropyBytes wl d = runReader entropyM wl
        where entropyM :: DbMonad Double
              entropyM = sum <$> (map VB.cardEntropy <$> cardinalities d)

runCtxM :: CtxM a -> WordList -> a
runCtxM m wl = evalState (runReaderT m wl) Map.empty

encode :: WordList -> Document -> BS.ByteString -> (String, [VB.Word8])
encode wl fmt d = runCtxM encodeM wl
    where encodeM :: CtxM (String, [VB.Word8])
          encodeM = do
            cs <- cardinalities fmt
            let (ws, d') = VB.encode cs (BS.unpack d) in
                do
                    encoded <- encodeWords fmt (ws ++ [0,0..])
                    return (encoded, d')

-- abbreviation to annotate input stream in type signatures
type Input = String

-- decodes a type from the beginning of the input
-- returns the decoded word, its index in the type, and the remaining input
decodeType :: Type -> Input -> CtxMT [] (String, Int, Input)
decodeType t s = do
    ws <- getWords t
    let possibleParses = catMaybes $ zipWith parse [0..] ws in
        liftPossibilities possibleParses
    where parse :: Int -> String -> Maybe (String, Int, Input)
          parse i w = do
            rest <- stripPrefix w s
            return (w, i, rest :: Input)

decodePlaceholder :: Placeholder -> Input -> CtxMT [] (Int, Input)
decodePlaceholder ph s = do
    (w, i, rest) <- decodeType (placeholderType ph) s
    case ph of
        Binding n _ -> addBinding n w >> return (i, rest)
        _ -> return (i, rest)

decodeToken :: Token -> Input -> CtxMT [] (Int, Input)
decodeToken t s = case t of
    Comment _ -> return (0, s)
    Literal ls -> decodePlaceholder (PlainType (OneOf [ls])) s
    Placeholder ph -> decodePlaceholder ph s

decodeDocument :: Document -> Input -> CtxMT [] ([Int], Input)
decodeDocument d s =
    case d of
        [] -> return ([], s)
        t:d' -> do
            (i, s) <- decodeToken t s
            (is, s) <- decodeDocument d' s
            return (i:is, s)

decodeDocumentData :: Document -> Input -> CtxMT [] ([VB.Word8], Input)
decodeDocumentData d s = do
    cards <- cardinalities d
    (ws, s) <- decodeDocument d s
    let (bytes, ws') = VB.decode cards ws in
        if ws' /= []
            then liftPossibilities []
            else return (bytes, s)

runCtxMT :: Monad m => CtxMT m a -> WordList -> m a
runCtxMT m wl = evalStateT (runReaderT m wl) Map.empty

data DecodeError =
    NoParse
    | IncompleteParse String
    | AmbiguousParse [BS.ByteString]
    deriving (Eq, Show)

completeParse :: (a, Input) -> Maybe a
completeParse (x, s) = if null s then Just x else Nothing

decode :: WordList -> Document -> String -> Either DecodeError BS.ByteString
decode wl d s =
    let parses = runCtxMT (decodeDocumentData d s) wl
        completeParses = BS.pack <$> mapMaybe completeParse parses in
        case parses of
            [(p, s)] | s /= [] -> Left $ IncompleteParse s
            _ -> case completeParses of
                [] -> Left NoParse
                [p] -> let numBytes = floor (entropyBytes wl d) in
                          Right (BS.take numBytes p)
                _ -> Left $ AmbiguousParse completeParses
