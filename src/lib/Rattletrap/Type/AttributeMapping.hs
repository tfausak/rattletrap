{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word32le
  , attributeMappingStreamId :: Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''AttributeMapping)

putAttributeMapping :: AttributeMapping -> BytePut
putAttributeMapping attributeMapping = do
  putWord32 (attributeMappingObjectId attributeMapping)
  putWord32 (attributeMappingStreamId attributeMapping)

decodeAttributeMapping :: ByteGet AttributeMapping
decodeAttributeMapping =
  AttributeMapping <$> decodeWord32le <*> decodeWord32le
