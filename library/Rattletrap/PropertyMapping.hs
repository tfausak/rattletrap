module Rattletrap.PropertyMapping where

import Rattletrap.Word32

import qualified Data.Binary as Binary

data PropertyMapping = PropertyMapping
  { propertyMappingObjectId :: Word32
  , propertyMappingStreamId :: Word32
  } deriving (Eq, Ord, Show)

getPropertyMapping :: Binary.Get PropertyMapping
getPropertyMapping = do
  objectId <- getWord32
  streamId <- getWord32
  pure
    PropertyMapping
    {propertyMappingObjectId = objectId, propertyMappingStreamId = streamId}

putPropertyMapping :: PropertyMapping -> Binary.Put
putPropertyMapping propertyMapping = do
  putWord32 (propertyMappingObjectId propertyMapping)
  putWord32 (propertyMappingStreamId propertyMapping)
