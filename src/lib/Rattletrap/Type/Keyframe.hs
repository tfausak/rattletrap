module Rattletrap.Type.Keyframe where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data Keyframe = Keyframe
  { time :: F32.F32
  -- ^ When this key frame occurs, in seconds.
  , frame :: U32.U32
  -- ^ The frame number of this key frame, starting from 0.
  , position :: U32.U32
  -- ^ The bit position of this key frame in the stream.
  }
  deriving (Eq, Show)

instance Json.FromValue Keyframe where
  fromValue = Json.withObject "Keyframe" $ \object -> do
    time <- Json.required object "time"
    frame <- Json.required object "frame"
    position <- Json.required object "position"
    pure Keyframe { time, frame, position }

instance Json.ToValue Keyframe where
  toValue x = Json.object
    [ Json.pair "time" $ time x
    , Json.pair "frame" $ frame x
    , Json.pair "position" $ position x
    ]

schema :: Schema.Schema
schema = Schema.named "keyframe" $ Schema.object
  [ (Json.pair "time" $ Schema.ref F32.schema, True)
  , (Json.pair "frame" $ Schema.ref U32.schema, True)
  , (Json.pair "position" $ Schema.ref U32.schema, True)
  ]

bytePut :: Keyframe -> BytePut.BytePut
bytePut x =
  F32.bytePut (time x) <> U32.bytePut (frame x) <> U32.bytePut (position x)

byteGet :: ByteGet.ByteGet Keyframe
byteGet = ByteGet.label "Keyframe" $ do
  time <- ByteGet.label "time" F32.byteGet
  frame <- ByteGet.label "frame" U32.byteGet
  position <- ByteGet.label "position" U32.byteGet
  pure Keyframe { time, frame, position }
