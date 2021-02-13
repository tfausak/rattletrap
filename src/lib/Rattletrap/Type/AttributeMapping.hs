{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word32le.Word32le
  , attributeMappingStreamId :: Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''AttributeMapping)

putAttributeMapping :: AttributeMapping -> BytePut
putAttributeMapping attributeMapping = do
  Word32le.bytePut (attributeMappingObjectId attributeMapping)
  Word32le.bytePut (attributeMappingStreamId attributeMapping)

decodeAttributeMapping :: ByteGet AttributeMapping
decodeAttributeMapping =
  AttributeMapping <$> Word32le.byteGet <*> Word32le.byteGet
