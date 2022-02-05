module Rattletrap.Type.Keyframe where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data Keyframe = Keyframe
  { time :: F32.F32
  -- ^ When this key frame occurs, in seconds.
  , frame :: U32.U32
  -- ^ The frame number of this key frame, starting from 0.
  , position :: U32.U32
  -- ^ The bit position of this key frame in the stream.
  }
  deriving (Eq, Show)

instance Argo.HasCodec Keyframe where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ Keyframe
      <$> Argo.project time (Argo.required (Argo.fromString "time") Argo.codec)
      <*> Argo.project
            frame
            (Argo.required (Argo.fromString "frame") Argo.codec)
      <*> Argo.project
            position
            (Argo.required (Argo.fromString "position") Argo.codec)

bytePut :: Keyframe -> BytePut.BytePut
bytePut x =
  F32.bytePut (time x) <> U32.bytePut (frame x) <> U32.bytePut (position x)

byteGet :: ByteGet.ByteGet Keyframe
byteGet = ByteGet.label "Keyframe" $ do
  time <- ByteGet.label "time" F32.byteGet
  frame <- ByteGet.label "frame" U32.byteGet
  position <- ByteGet.label "position" U32.byteGet
  pure Keyframe { time, frame, position }
