module Rattletrap.Type.Mark where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data Mark = Mark
  { value :: Str.Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , frame :: U32.U32
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Show)

instance Json.FromJSON Mark where
  parseJSON = Json.withObject "Mark" $ \object -> do
    value <- Json.required object "value"
    frame <- Json.required object "frame"
    pure Mark { value, frame }

instance Json.ToJSON Mark where
  toJSON x =
    Json.object [Json.pair "value" $ value x, Json.pair "frame" $ frame x]

schema :: Schema.Schema
schema = Schema.named "mark" $ Schema.object
  [ (Json.pair "value" $ Schema.ref Str.schema, True)
  , (Json.pair "frame" $ Schema.ref U32.schema, True)
  ]

bytePut :: Mark -> BytePut.BytePut
bytePut x = Str.bytePut (value x) <> U32.bytePut (frame x)

byteGet :: ByteGet.ByteGet Mark
byteGet = do
  value <- Str.byteGet
  frame <- U32.byteGet
  pure Mark { value, frame }
