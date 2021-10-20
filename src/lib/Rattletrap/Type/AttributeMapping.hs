module Rattletrap.Type.AttributeMapping where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

instance Json.FromValue AttributeMapping where
  fromValue = Json.withObject "AttributeMapping" $ \object -> do
    objectId <- Json.required object "object_id"
    streamId <- Json.required object "stream_id"
    pure AttributeMapping { objectId, streamId }

instance Json.ToValue AttributeMapping where
  toValue x = Json.object
    [Json.pair "object_id" $ objectId x, Json.pair "stream_id" $ streamId x]

schema :: Schema.Schema
schema = Schema.named "attributeMapping" $ Schema.object
  [ (Json.pair "object_id" $ Schema.ref U32.schema, True)
  , (Json.pair "stream_id" $ Schema.ref U32.schema, True)
  ]

bytePut :: AttributeMapping -> BytePut.BytePut
bytePut x = U32.bytePut (objectId x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet AttributeMapping
byteGet = ByteGet.label "AttributeMapping" $ do
  objectId <- ByteGet.label "objectId" U32.byteGet
  streamId <- ByteGet.label "streamId" U32.byteGet
  pure AttributeMapping { objectId, streamId }
