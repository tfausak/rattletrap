module Rattletrap.Type.Cache where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import Rattletrap.Type.Common
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: List.List AttributeMapping.AttributeMapping
  }
  deriving (Eq, Show)

$(deriveJson ''Cache)

bytePut :: Cache -> BytePut.BytePut
bytePut x =
  U32.bytePut (classId x)
    <> U32.bytePut (parentCacheId x)
    <> U32.bytePut (cacheId x)
    <> List.bytePut AttributeMapping.bytePut (attributeMappings x)

byteGet :: ByteGet.ByteGet Cache
byteGet =
  Cache
    <$> U32.byteGet
    <*> U32.byteGet
    <*> U32.byteGet
    <*> List.byteGet AttributeMapping.byteGet
