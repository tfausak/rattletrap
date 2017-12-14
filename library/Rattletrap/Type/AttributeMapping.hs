{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.AttributeMapping
  ( AttributeMapping(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32

data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word32
  , attributeMappingStreamId :: Word32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON AttributeMapping where
  parseJSON = defaultParseJson "AttributeMapping"

instance ToJSON AttributeMapping where
  toEncoding = defaultToEncoding "AttributeMapping"
  toJSON = defaultToJson "AttributeMapping"
