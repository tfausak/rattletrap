module Rattletrap.Type.ClassMapping where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data ClassMapping = ClassMapping
  { name :: Str.Str
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

instance Json.FromValue ClassMapping where
  fromValue = Json.withObject "ClassMapping" $ \object -> do
    name <- Json.required object "name"
    streamId <- Json.required object "stream_id"
    pure ClassMapping { name, streamId }

instance Json.ToValue ClassMapping where
  toValue x =
    Json.object [Json.pair "name" $ name x, Json.pair "stream_id" $ streamId x]

schema :: Schema.Schema
schema = Schema.named "classMapping" $ Schema.object
  [ (Json.pair "name" $ Schema.ref Str.schema, True)
  , (Json.pair "stream_id" $ Schema.ref U32.schema, True)
  ]

bytePut :: ClassMapping -> BytePut.BytePut
bytePut x = Str.bytePut (name x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet ClassMapping
byteGet = ByteGet.label "ClassMapping" $ do
  name <- ByteGet.label "name" Str.byteGet
  streamId <- ByteGet.label "streamId" U32.byteGet
  pure ClassMapping { name, streamId }
