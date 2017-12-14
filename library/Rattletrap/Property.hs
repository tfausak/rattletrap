module Rattletrap.Property where

import Rattletrap.Type.Text
import Rattletrap.Decode.Text
import Rattletrap.Encode.Text
import Rattletrap.Type.Word64
import Rattletrap.Decode.Word64
import Rattletrap.Encode.Word64
import Rattletrap.PropertyValue

import qualified Data.Binary as Binary

data Property = Property
  { propertyKind :: Text
  , propertySize :: Word64
  -- ^ Not used.
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
