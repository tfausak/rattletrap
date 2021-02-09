module Rattletrap.Encode.Rotation
  ( putRotation
  ) where

import Rattletrap.Encode.CompressedWordVector
import Rattletrap.Encode.Quaternion
import Rattletrap.Type.Rotation

import qualified Data.Binary.Bits.Put as BinaryBits

putRotation :: Rotation -> BinaryBits.BitPut ()
putRotation r = case r of
  RotationCompressedWordVector cwv -> putCompressedWordVector cwv
  RotationQuaternion q -> putQuaternion q
