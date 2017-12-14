module Rattletrap.Encode.KeyFrame
  ( putKeyFrame
  ) where

import Rattletrap.Type.KeyFrame
import Rattletrap.Encode.Float32
import Rattletrap.Encode.Word32

import qualified Data.Binary as Binary

putKeyFrame :: KeyFrame -> Binary.Put
putKeyFrame keyFrame = do
  putFloat32 (keyFrameTime keyFrame)
  putWord32 (keyFrameFrame keyFrame)
  putWord32 (keyFramePosition keyFrame)
