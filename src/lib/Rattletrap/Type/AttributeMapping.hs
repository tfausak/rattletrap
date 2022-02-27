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
      <$> Argo.required objectId "object_id"
      <*> Argo.required streamId "stream_id"

bytePut :: AttributeMapping -> BytePut.BytePut
bytePut x = U32.bytePut (objectId x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet AttributeMapping
byteGet = ByteGet.label "AttributeMapping" $ do
  objectId <- ByteGet.label "objectId" U32.byteGet
  streamId <- ByteGet.label "streamId" U32.byteGet
  pure AttributeMapping { objectId, streamId }
