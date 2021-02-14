{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Cache where

import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import Rattletrap.Type.Common
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import qualified Rattletrap.BytePut as BytePut

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: List.List AttributeMapping.AttributeMapping
  }
  deriving (Eq, Show)

$(deriveJson ''Cache)

bytePut :: Cache -> BytePut.BytePut
bytePut cache = do
  U32.bytePut (classId cache)
  U32.bytePut (parentCacheId cache)
  U32.bytePut (cacheId cache)
  List.bytePut AttributeMapping.bytePut (attributeMappings cache)

byteGet :: ByteGet Cache
byteGet =
  Cache
    <$> U32.byteGet
    <*> U32.byteGet
    <*> U32.byteGet
    <*> List.byteGet AttributeMapping.byteGet
