{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.PropertyMapping where

import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data PropertyMapping = PropertyMapping
  { propertyMappingObjectId :: Word32
  , propertyMappingStreamId :: Word32
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON PropertyMapping

instance Aeson.ToJSON PropertyMapping

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
