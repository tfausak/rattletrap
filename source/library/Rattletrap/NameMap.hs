module Rattletrap.NameMap where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap

newtype NameMap = NameMap
  { nameMapValue :: HashMap.HashMap Int Text
  } deriving (Eq, Show)

makeNameMap :: List Text -> NameMap
makeNameMap names = NameMap (HashMap.fromList (zip [0 ..] (listValue names)))

nameMapLookup :: Word32 -> NameMap -> Maybe Text
nameMapLookup nameIndex (NameMap nameMap) =
  HashMap.lookup (fromIntegral (word32Value nameIndex)) nameMap
