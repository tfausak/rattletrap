module Rattletrap.KeyFrame where

import Rattletrap.Type.Float32
import Rattletrap.Decode.Float32
import Rattletrap.Encode.Float32
import Rattletrap.Type.Word32
import Rattletrap.Decode.Word32
import Rattletrap.Encode.Word32

import qualified Data.Binary as Binary

data KeyFrame = KeyFrame
  { keyFrameTime :: Float32
  -- ^ When this key frame occurs, in seconds.
  , keyFrameFrame :: Word32
  -- ^ The frame number of this key frame, starting from 0.
  , keyFramePosition :: Word32
  -- ^ The bit position of this key frame in the stream.
  } deriving (Eq, Ord, Show)

getKeyFrame :: Binary.Get KeyFrame
getKeyFrame = do
  time <- getFloat32
  frame <- getWord32
  position <- getWord32
  pure (KeyFrame time frame position)

putKeyFrame :: KeyFrame -> Binary.Put
putKeyFrame keyFrame = do
  putFloat32 (keyFrameTime keyFrame)
  putWord32 (keyFrameFrame keyFrame)
  putWord32 (keyFramePosition keyFrame)
