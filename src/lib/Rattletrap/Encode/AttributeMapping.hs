module Rattletrap.Encode.AttributeMapping
  ( putAttributeMapping
  ) where

import Rattletrap.Encode.Word32le
import Rattletrap.Type.AttributeMapping

import qualified Data.Binary as Binary

putAttributeMapping :: AttributeMapping -> Binary.Put
putAttributeMapping attributeMapping = do
  putWord32 (attributeMappingObjectId attributeMapping)
  putWord32 (attributeMappingStreamId attributeMapping)
