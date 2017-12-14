{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Cache
  ( Cache(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.Word32
import Rattletrap.Type.List

data Cache = Cache
  { cacheClassId :: Word32
  , cacheParentCacheId :: Word32
  , cacheCacheId :: Word32
  , cacheAttributeMappings :: List AttributeMapping
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Cache where
  parseJSON = defaultParseJson "Cache"

instance ToJSON Cache where
  toEncoding = defaultToEncoding "Cache"
  toJSON = defaultToJson "Cache"
