module Rattletrap.PropertyMapping where

import Rattletrap.Word32

import qualified Data.Binary as Binary

data PropertyMapping = PropertyMapping
  { propertyObjectId :: Word32
  , propertyMappingStreamId :: Word32
  } deriving (Eq, Ord, Show)

getPropertyMapping :: Binary.Get PropertyMapping
getPropertyMapping = do
  objectId <- getWord32
  streamId <- getWord32
  pure
    PropertyMapping
    {propertyObjectId = objectId, propertyMappingStreamId = streamId}

putPropertyMapping :: PropertyMapping -> Binary.Put
putPropertyMapping propertyMapping = do
  putWord32 (propertyObjectId propertyMapping)
  putWord32 (propertyMappingStreamId propertyMapping)
