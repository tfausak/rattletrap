{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le

import qualified Data.Binary as Binary

data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word32le
  , attributeMappingStreamId :: Word32le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''AttributeMapping)

putAttributeMapping :: AttributeMapping -> Binary.Put
putAttributeMapping attributeMapping = do
  putWord32 (attributeMappingObjectId attributeMapping)
  putWord32 (attributeMappingStreamId attributeMapping)
