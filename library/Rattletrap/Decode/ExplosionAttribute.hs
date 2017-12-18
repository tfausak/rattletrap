module Rattletrap.Decode.ExplosionAttribute
  ( decodeExplosionAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Type.ExplosionAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeExplosionAttributeBits :: DecodeBits ExplosionAttribute
decodeExplosionAttributeBits =
  ExplosionAttribute
    <$> BinaryBits.getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits
