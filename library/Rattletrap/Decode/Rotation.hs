module Rattletrap.Decode.Rotation
  ( decodeRotationBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWordVector
import Rattletrap.Decode.Quaternion
import Rattletrap.Type.Rotation

decodeRotationBits :: (Int, Int, Int) -> DecodeBits Rotation
decodeRotationBits version = if version >= (868, 22, 7)
  then RotationQuaternion <$> decodeQuaternionBits
  else RotationCompressedWordVector <$> decodeCompressedWordVectorBits
