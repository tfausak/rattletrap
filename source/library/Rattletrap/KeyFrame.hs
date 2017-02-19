module Rattletrap.KeyFrame where

import Rattletrap.Primitive

import qualified Data.Binary as Binary

data KeyFrame = KeyFrame
  { keyFrameTime :: Float32
  -- ^ When this key frame occurs, in seconds.
  , keyFrameFrame :: Word32
  -- ^ The frame number of this key frame, starting from 0.
  , keyFramePosition :: Word32
  -- ^ The bit position of this key frame in the stream.
  } deriving (Eq, Show)

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
