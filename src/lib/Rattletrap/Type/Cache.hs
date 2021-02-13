{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Cache where

import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.Common
import Rattletrap.Type.List
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Cache = Cache
  { cacheClassId :: Word32le.Word32le
  , cacheParentCacheId :: Word32le.Word32le
  , cacheCacheId :: Word32le.Word32le
  , cacheAttributeMappings :: List AttributeMapping
  }
  deriving (Eq, Show)

$(deriveJson ''Cache)

putCache :: Cache -> BytePut
putCache cache = do
  Word32le.bytePut (cacheClassId cache)
  Word32le.bytePut (cacheParentCacheId cache)
  Word32le.bytePut (cacheCacheId cache)
  putList putAttributeMapping (cacheAttributeMappings cache)

decodeCache :: ByteGet Cache
decodeCache =
  Cache
    <$> Word32le.byteGet
    <*> Word32le.byteGet
    <*> Word32le.byteGet
    <*> decodeList decodeAttributeMapping
