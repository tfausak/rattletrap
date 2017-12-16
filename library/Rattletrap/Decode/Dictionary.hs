module Rattletrap.Decode.Dictionary
  ( decodeDictionary
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Str

decodeDictionary :: Decode a -> Decode (Dictionary a)
decodeDictionary decodeValue = do
  key <- getText
  case filter (/= '\x00') (fromStr key) of
    "None" -> pure (DictionaryEnd key)
    _ ->
      DictionaryElement key <$> decodeValue <*> decodeDictionary decodeValue
