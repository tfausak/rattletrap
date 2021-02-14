{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Cache where

import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import Rattletrap.Type.Common
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Cache = Cache
  { classId :: Word32le.Word32le
  , parentCacheId :: Word32le.Word32le
  , cacheId :: Word32le.Word32le
  , attributeMappings :: List.List AttributeMapping.AttributeMapping
  }
  deriving (Eq, Show)

$(deriveJson ''Cache)

bytePut :: Cache -> BytePut
bytePut cache = do
  Word32le.bytePut (classId cache)
  Word32le.bytePut (parentCacheId cache)
  Word32le.bytePut (cacheId cache)
  List.bytePut AttributeMapping.bytePut (attributeMappings cache)

byteGet :: ByteGet Cache
byteGet =
  Cache
    <$> Word32le.byteGet
    <*> Word32le.byteGet
    <*> Word32le.byteGet
    <*> List.byteGet AttributeMapping.byteGet
