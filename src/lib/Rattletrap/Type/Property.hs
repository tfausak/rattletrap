{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Property where

import Rattletrap.Type.Common
import Rattletrap.Type.PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word64le as Word64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Property = Property
  { propertyKind :: Str.Str
  , propertySize :: Word64le.Word64le
  -- ^ Not used.
  , propertyValue :: PropertyValue Property
  }
  deriving (Eq, Show)

$(deriveJson ''Property)

putProperty :: Property -> BytePut
putProperty property = do
  Str.bytePut (propertyKind property)
  Word64le.bytePut (propertySize property)
  putPropertyValue putProperty (propertyValue property)

decodeProperty :: ByteGet Property
decodeProperty = do
  kind <- Str.byteGet
  Property kind <$> Word64le.byteGet <*> decodePropertyValue decodeProperty kind
