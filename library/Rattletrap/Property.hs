module Rattletrap.Property where

import Rattletrap.PropertyValue
import Rattletrap.Primitive.Text
import Rattletrap.Primitive.Word64

import qualified Data.Binary as Binary

data Property = Property
  { propertyKind :: Text
  , propertySize :: Word64
  , propertyValue :: PropertyValue Property
  } deriving (Eq, Ord, Show)

getProperty :: Binary.Get Property
getProperty = do
  kind <- getText
  size <- getWord64
  value <- getPropertyValue getProperty kind
  pure (Property kind size value)

putProperty :: Property -> Binary.Put
putProperty property = do
  putText (propertyKind property)
  putWord64 (propertySize property)
  putPropertyValue putProperty (propertyValue property)
