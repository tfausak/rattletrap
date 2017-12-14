module Rattletrap.Encode.Dictionary
  ( putDictionary
  ) where

import Rattletrap.Primitive.Text
import Rattletrap.Type.Dictionary

import qualified Data.Binary as Binary
import qualified Data.Map as Map

putDictionary :: (a -> Binary.Put) -> Dictionary a -> Binary.Put
putDictionary putValue dictionary = do
  let elements = dictionaryValue dictionary
  mapM_
    ( \key -> do
      putText key
      case Map.lookup (textValue key) elements of
        Nothing -> fail ("could not find key " ++ textToString key)
        Just value -> putValue value
    )
    (dictionaryKeys dictionary)
  putText (dictionaryLastKey dictionary)
