module Rattletrap.Decode.ExplosionAttribute
  ( decodeExplosionAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Type.ExplosionAttribute

decodeExplosionAttributeBits :: (Int, Int, Int) -> DecodeBits ExplosionAttribute
decodeExplosionAttributeBits version =
  ExplosionAttribute <$> getBool <*> decodeInt32leBits <*> decodeVectorBits version
