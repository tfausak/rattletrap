module Rattletrap.Decode.RigidBodyStateAttribute
  ( decodeRigidBodyStateAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWordVector
import Rattletrap.Decode.Vector
import Rattletrap.Type.RigidBodyStateAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeRigidBodyStateAttributeBits :: DecodeBits RigidBodyStateAttribute
decodeRigidBodyStateAttributeBits = do
  sleeping <- BinaryBits.getBool
  RigidBodyStateAttribute sleeping
    <$> decodeVectorBits
    <*> decodeCompressedWordVectorBits
    <*> decodeWhen (not sleeping) decodeVectorBits
    <*> decodeWhen (not sleeping) decodeVectorBits
