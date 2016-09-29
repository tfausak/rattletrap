module Rattletrap.Cache where

import Rattletrap.List
import Rattletrap.PropertyMapping
import Rattletrap.Word32

import qualified Data.Binary as Binary

data Cache = Cache
  { cacheClassId :: Word32
  , cacheParentCacheId :: Word32
  , cacheCacheId :: Word32
  , cachePropertyMappings :: List PropertyMapping
  } deriving (Eq, Ord, Show)

getMapping :: Binary.Get Cache
getMapping = do
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

putMapping :: Cache -> Binary.Put
putMapping cache = do
  putWord32 (cacheClassId cache)
  putWord32 (cacheParentCacheId cache)
  putWord32 (cacheCacheId cache)
  putList putPropertyMapping (cachePropertyMappings cache)
