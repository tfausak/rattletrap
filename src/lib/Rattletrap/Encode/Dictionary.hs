module Rattletrap.Encode.Dictionary
  ( putDictionary
  ) where

import Rattletrap.Encode.Str
import Rattletrap.Type.Dictionary

import qualified Data.Binary as Binary

putDictionary :: (a -> Binary.Put) -> Dictionary a -> Binary.Put
putDictionary f x = case x of
  DictionaryElement k v y -> do
    putText k
    f v
    putDictionary f y
  DictionaryEnd y -> putText y
