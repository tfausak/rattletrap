module Rattletrap.Type.AttributeMapping where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

instance Argo.HasCodec AttributeMapping where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ AttributeMapping
      <$> Argo.project
            objectId
            (Argo.required (Argo.fromString "object_id") Argo.codec)
      <*> Argo.project
            streamId
            (Argo.required (Argo.fromString "stream_id") Argo.codec)

bytePut :: AttributeMapping -> BytePut.BytePut
bytePut x = U32.bytePut (objectId x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet AttributeMapping
byteGet = ByteGet.label "AttributeMapping" $ do
  objectId <- ByteGet.label "objectId" U32.byteGet
  streamId <- ByteGet.label "streamId" U32.byteGet
  pure AttributeMapping { objectId, streamId }
