module Rattletrap.Type.AttributeMapping where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

$(deriveJson ''AttributeMapping)

schema :: Schema.Schema
schema = Schema.named "attributeMapping" $ Schema.object
  [ (Json.pair "object_id" $ Schema.ref U32.schema, True)
  , (Json.pair "stream_id" $ Schema.ref U32.schema, True)
  ]

bytePut :: AttributeMapping -> BytePut.BytePut
bytePut x = U32.bytePut (objectId x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet AttributeMapping
byteGet = AttributeMapping <$> U32.byteGet <*> U32.byteGet
