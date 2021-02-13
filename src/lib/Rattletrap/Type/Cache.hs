{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Cache where

import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.Common
import Rattletrap.Type.List
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary

data Cache = Cache
  { cacheClassId :: Word32le
  , cacheParentCacheId :: Word32le
  , cacheCacheId :: Word32le
  , cacheAttributeMappings :: List AttributeMapping
  }
  deriving (Eq, Show)

$(deriveJson ''Cache)

putCache :: Cache -> Binary.Put
putCache cache = do
  putWord32 (cacheClassId cache)
  putWord32 (cacheParentCacheId cache)
  putWord32 (cacheCacheId cache)
  putList putAttributeMapping (cacheAttributeMappings cache)

decodeCache :: ByteGet Cache
decodeCache =
  Cache
    <$> decodeWord32le
    <*> decodeWord32le
    <*> decodeWord32le
    <*> decodeList decodeAttributeMapping
