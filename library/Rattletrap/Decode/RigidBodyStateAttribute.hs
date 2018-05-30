module Rattletrap.Decode.RigidBodyStateAttribute
  ( decodeRigidBodyStateAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWordVector
import Rattletrap.Decode.Vector
import Rattletrap.Type.RigidBodyStateAttribute

decodeRigidBodyStateAttributeBits :: (Int, Int, Int) -> DecodeBits RigidBodyStateAttribute
decodeRigidBodyStateAttributeBits version = do
  sleeping <- getBool
  RigidBodyStateAttribute sleeping
    <$> decodeVectorBits version
    <*> decodeCompressedWordVectorBits
    <*> decodeWhen (not sleeping) (decodeVectorBits version)
    <*> decodeWhen (not sleeping) (decodeVectorBits version)
