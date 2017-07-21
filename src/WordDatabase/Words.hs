module WordDatabase.Words where

import Syntax
import qualified Data.Map.Strict as Map

type WordList = Map.Map Type [String]

getWords :: WordList -> Type -> [String]
getWords l t = Map.findWithDefault ["(" ++ show t ++ ")"] t l
