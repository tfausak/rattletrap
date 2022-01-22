module Rattletrap.Type.Cache where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: List.List AttributeMapping.AttributeMapping
  }
  deriving (Eq, Show)

instance Argo.HasCodec Cache where
  codec =
    Argo.fromObjectCodec Argo.Allow
      $ Cache
      <$> Argo.project
            classId
            (Argo.required (Argo.fromString "class_id") Argo.codec)
      <*> Argo.project
            parentCacheId
            (Argo.required (Argo.fromString "parent_cache_id") Argo.codec)
      <*> Argo.project
            cacheId
            (Argo.required (Argo.fromString "cache_id") Argo.codec)
      <*> Argo.project
            attributeMappings
            (Argo.required (Argo.fromString "attribute_mappings") Argo.codec)

bytePut :: Cache -> BytePut.BytePut
bytePut x =
  U32.bytePut (classId x)
    <> U32.bytePut (parentCacheId x)
    <> U32.bytePut (cacheId x)
    <> List.bytePut AttributeMapping.bytePut (attributeMappings x)

byteGet :: ByteGet.ByteGet Cache
byteGet = ByteGet.label "Cache" $ do
  classId <- ByteGet.label "classId" U32.byteGet
  parentCacheId <- ByteGet.label "parentCacheId" U32.byteGet
  cacheId <- ByteGet.label "cacheId" U32.byteGet
  attributeMappings <- ByteGet.label "attributeMappings"
    $ List.byteGet AttributeMapping.byteGet
  pure Cache { classId, parentCacheId, cacheId, attributeMappings }
