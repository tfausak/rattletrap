module Rattletrap.Type.Message where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
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

instance Json.FromJSON Message where
  parseJSON = Json.withObject "Message" $ \object -> do
    frame <- Json.required object "frame"
    name <- Json.required object "name"
    value <- Json.required object "value"
    pure Message { frame, name, value }

instance Json.ToJSON Message where
  toJSON x = Json.object
    [ Json.pair "frame" $ frame x
    , Json.pair "name" $ name x
    , Json.pair "value" $ value x
    ]

schema :: Schema.Schema
schema = Schema.named "message" $ Schema.object
  [ (Json.pair "frame" $ Schema.ref U32.schema, True)
  , (Json.pair "name" $ Schema.ref Str.schema, True)
  , (Json.pair "value" $ Schema.ref Str.schema, True)
  ]

bytePut :: Message -> BytePut.BytePut
bytePut x =
  U32.bytePut (frame x) <> Str.bytePut (name x) <> Str.bytePut (value x)

byteGet :: ByteGet.ByteGet Message
byteGet = do
  frame <- U32.byteGet
  name <- Str.byteGet
  value <- Str.byteGet
  pure Message { frame, name, value }
