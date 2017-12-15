module Rattletrap.Decode.Cache
  ( getCache
  ) where

import Rattletrap.Decode.AttributeMapping
import Rattletrap.Decode.List
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Cache

import qualified Data.Binary as Binary

getCache :: Binary.Get Cache
getCache = do
  classId <- getWord32
  parentCacheId <- getWord32
  cacheId <- getWord32
  attributeMappings <- getList getAttributeMapping
  pure (Cache classId parentCacheId cacheId attributeMappings)
