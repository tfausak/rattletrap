{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.KeyFrame where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data KeyFrame = KeyFrame
  { keyFrameTime :: Float32le
  -- ^ When this key frame occurs, in seconds.
  , keyFrameFrame :: Word32le.Word32le
  -- ^ The frame number of this key frame, starting from 0.
  , keyFramePosition :: Word32le.Word32le
  -- ^ The bit position of this key frame in the stream.
  }
  deriving (Eq, Show)

$(deriveJson ''KeyFrame)

putKeyFrame :: KeyFrame -> BytePut
putKeyFrame keyFrame = do
  putFloat32 (keyFrameTime keyFrame)
  Word32le.bytePut (keyFrameFrame keyFrame)
  Word32le.bytePut (keyFramePosition keyFrame)

decodeKeyFrame :: ByteGet KeyFrame
decodeKeyFrame =
  KeyFrame <$> decodeFloat32le <*> Word32le.byteGet <*> Word32le.byteGet
