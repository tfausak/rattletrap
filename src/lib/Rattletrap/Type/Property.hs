{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Property where

import Rattletrap.Type.Common
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Str
import Rattletrap.Type.Word64le

import qualified Data.Binary as Binary

data Property = Property
  { propertyKind :: Str
  , propertySize :: Word64le
  -- ^ Not used.
  , propertyValue :: PropertyValue Property
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Property)

putProperty :: Property -> Binary.Put
putProperty property = do
  putText (propertyKind property)
  putWord64 (propertySize property)
  putPropertyValue putProperty (propertyValue property)
