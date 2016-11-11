module Rattletrap.KeyFrame where

import Rattletrap.Primitive.Float32
import Rattletrap.Word32

import qualified Data.Binary as Binary

data KeyFrame = KeyFrame
  { keyFrameTime :: Float32
  , keyFrameFrame :: Word32
  , keyFramePosition :: Word32
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
