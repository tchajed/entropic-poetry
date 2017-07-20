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

type WordDatabase = Type -> [String]

type Ctx = Map.Map Name String

type EncM a = State Ctx a

lookupName :: Name -> EncM String
lookupName name@(Name n) =
    let def = "{?" ++ n ++ "}" in
    gets (Map.findWithDefault def name)

getWords :: WordDatabase -> Type -> EncM [String]
getWords db t =
    case t of
        Reference n -> do
            w <- lookupName n
            return [w]
        OneOf ws -> return ws
        _ -> return $ db t

-- this could be length <$> getWords db t, but we can implement this without a
-- context
typeCard :: WordDatabase -> Type -> Int
typeCard db t =
    case t of
        Reference n -> 1
        OneOf ws -> length ws
        _ -> length $ db t

addBinding :: Name -> String -> EncM ()
addBinding n s = modify (Map.insert n s)

placeholderType :: Placeholder -> Type
placeholderType (PlainType t) = t
placeholderType (Binding _ t) = t

encodeWord :: WordDatabase -> Placeholder -> Int -> EncM String
encodeWord db ph w = do
    s <- (!! w) <$> getWords db (placeholderType ph)
    case ph of
        Binding n _ -> addBinding n s >> return s
        _ -> return s

encodeToken :: WordDatabase -> Token -> Int -> EncM String
encodeToken db t w =
    case t of
        Comment _ -> return ""
        Literal s -> return s
        Placeholder ph -> encodeWord db ph w

encodeWords :: WordDatabase -> Document -> [Int] -> String
encodeWords db d ws = evalState (concat <$> wordsM) Map.empty
        where wordsM :: EncM [String]
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
