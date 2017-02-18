module Rattletrap.Map.Name
  ( NameMap
  , makeNameMap
  , nameMapLookup
  ) where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

newtype NameMap =
  NameMap (HashMap.HashMap Int Text)
  deriving (Eq, Show)

makeNameMap :: List Text -> NameMap
makeNameMap names =
  NameMap (Vector.ifoldr HashMap.insert HashMap.empty (listVector names))

nameMapLookup :: Word32 -> NameMap -> Maybe Text
nameMapLookup nameIndex (NameMap nameMap) =
  HashMap.lookup (fromIntegral (word32Value nameIndex)) nameMap
