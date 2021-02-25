module Rattletrap.Type.Cache where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import Rattletrap.Type.Common
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: List.List AttributeMapping.AttributeMapping
  }
  deriving (Eq, Show)

$(deriveJson ''Cache)

schema :: Schema.Schema
schema = Schema.named "cache" $ Schema.object
  [ (Json.pair "class_id" $ Schema.ref U32.schema, True)
  , (Json.pair "parent_cache_id" $ Schema.ref U32.schema, True)
  , (Json.pair "cache_id" $ Schema.ref U32.schema, True)
  , (Json.pair "attribute_mappings" . Schema.json $ List.schema AttributeMapping.schema, True)
  ]

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
