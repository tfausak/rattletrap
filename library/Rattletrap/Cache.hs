module Rattletrap.Cache where

import Rattletrap.AttributeMapping
import Rattletrap.Type.Word32
import Rattletrap.Decode.Word32
import Rattletrap.Encode.Word32
import Rattletrap.Type.List
import Rattletrap.Decode.List
import Rattletrap.Encode.List

import qualified Data.Binary as Binary

data Cache = Cache
  { cacheClassId :: Word32
  , cacheParentCacheId :: Word32
  , cacheCacheId :: Word32
  , cacheAttributeMappings :: List AttributeMapping
  } deriving (Eq, Ord, Show)

getCache :: Binary.Get Cache
getCache = do
  classId <- getWord32
  parentCacheId <- getWord32
  cacheId <- getWord32
  attributeMappings <- getList getAttributeMapping
  pure (Cache classId parentCacheId cacheId attributeMappings)

putCache :: Cache -> Binary.Put
putCache cache = do
  putWord32 (cacheClassId cache)
  putWord32 (cacheParentCacheId cache)
  putWord32 (cacheCacheId cache)
  putList putAttributeMapping (cacheAttributeMappings cache)
