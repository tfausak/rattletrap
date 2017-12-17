module Rattletrap.Decode.ExplosionAttribute
  ( decodeExplosionAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Type.ExplosionAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

decodeExplosionAttributeBits :: DecodeBits ExplosionAttribute
decodeExplosionAttributeBits =
  ExplosionAttribute
    <$> BinaryBit.getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits
