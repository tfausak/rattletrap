module Rattletrap.Encode.Dictionary
  ( putDictionary
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Encode.Str
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Str

import qualified Data.Binary as Binary
import qualified Data.Map as Map

putDictionary :: (a -> Binary.Put) -> Dictionary a -> Binary.Put
putDictionary putValue dictionary = do
  let elements = dictionaryValue dictionary
  mapM_
    ( \key -> do
      putText key
      case Map.lookup (strValue key) elements of
        Nothing -> fail ("could not find key " <> fromStr key)
        Just value -> putValue value
    )
    (dictionaryKeys dictionary)
  putText (dictionaryLastKey dictionary)
