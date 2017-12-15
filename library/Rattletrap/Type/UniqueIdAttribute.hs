{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.UniqueIdAttribute
  ( UniqueIdAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8
import Rattletrap.Type.RemoteId

data UniqueIdAttribute = UniqueIdAttribute
  { uniqueIdAttributeSystemId :: Word8
  , uniqueIdAttributeRemoteId :: RemoteId
  , uniqueIdAttributeLocalId :: Word8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON UniqueIdAttribute where
  parseJSON = defaultParseJson "UniqueIdAttribute"

instance ToJSON UniqueIdAttribute where
  toEncoding = defaultToEncoding "UniqueIdAttribute"
  toJSON = defaultToJson "UniqueIdAttribute"
