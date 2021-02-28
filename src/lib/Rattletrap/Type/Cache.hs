module Rattletrap.Type.Cache where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: List.List AttributeMapping.AttributeMapping
  }
  deriving (Eq, Show)

instance Json.FromJSON Cache where
  parseJSON = Json.withObject "Cache" $ \ object -> do
    classId <- Json.required object "class_id"
    parentCacheId <- Json.required object "parent_cache_id"
    cacheId <- Json.required object "cache_id"
    attributeMappings <- Json.required object "attribute_mappings"
    pure Cache { classId, parentCacheId, cacheId, attributeMappings }

instance Json.ToJSON Cache where
  toJSON x = Json.object
    [ Json.pair "class_id" $ classId x
    , Json.pair "parent_cache_id" $ parentCacheId x
    , Json.pair "cache_id" $ cacheId x
    , Json.pair "attribute_mappings" $ attributeMappings x
    ]

schema :: Schema.Schema
schema = Schema.named "cache" $ Schema.object
  [ (Json.pair "class_id" $ Schema.ref U32.schema, True)
  , (Json.pair "parent_cache_id" $ Schema.ref U32.schema, True)
  , (Json.pair "cache_id" $ Schema.ref U32.schema, True)
  , ( Json.pair "attribute_mappings" . Schema.json $ List.schema
      AttributeMapping.schema
    , True
    )
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
