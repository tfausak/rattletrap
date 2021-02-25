module Rattletrap.Type.Message where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

data Message = Message
  { frame :: U32.U32
  -- ^ Which frame this message belongs to, starting from 0.
  , name :: Str.Str
  -- ^ The primary player's name.
  , value :: Str.Str
  -- ^ The content of the message.
  }
  deriving (Eq, Show)

$(deriveJson ''Message)

schema :: Schema.Schema
schema = Schema.named "message" $ Schema.object
  [ (Json.pair "frame" $ Schema.ref U32.schema, True)
  , (Json.pair "name" $ Schema.ref Str.schema, True)
  , (Json.pair "value" $ Schema.ref Str.schema, True)
  ]

bytePut :: Message -> BytePut.BytePut
bytePut x =
  do
      U32.bytePut (frame x)
    <> Str.bytePut (name x)
    <> Str.bytePut (value x)

byteGet :: ByteGet.ByteGet Message
byteGet = Message <$> U32.byteGet <*> Str.byteGet <*> Str.byteGet
