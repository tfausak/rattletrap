module Rattletrap.Encode.KeyFrame
  ( putKeyFrame
  ) where

import Rattletrap.Encode.Float32le
import Rattletrap.Encode.Word32le
import Rattletrap.Type.KeyFrame

import qualified Data.Binary as Binary

putKeyFrame :: KeyFrame -> Binary.Put
putKeyFrame keyFrame = do
  putFloat32 (keyFrameTime keyFrame)
  putWord32 (keyFrameFrame keyFrame)
  putWord32 (keyFramePosition keyFrame)
