{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Cache where

import Rattletrap.List
import Rattletrap.PropertyMapping
import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data Cache = Cache
  { cacheClassId :: Word32
  , cacheParentCacheId :: Word32
  , cacheCacheId :: Word32
  , cachePropertyMappings :: List PropertyMapping
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Cache

instance Aeson.ToJSON Cache

getCache :: Binary.Get Cache
getCache = do
  classId <- getWord32
  parentCacheId <- getWord32
  cacheId <- getWord32
  propertyMappings <- getList getPropertyMapping
  pure
    Cache
    { cacheClassId = classId
    , cacheParentCacheId = parentCacheId
    , cacheCacheId = cacheId
    , cachePropertyMappings = propertyMappings
    }

putCache :: Cache -> Binary.Put
putCache cache = do
  putWord32 (cacheClassId cache)
  putWord32 (cacheParentCacheId cache)
  putWord32 (cacheCacheId cache)
  putList putPropertyMapping (cachePropertyMappings cache)
