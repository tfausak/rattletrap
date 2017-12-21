module Rattletrap.Decode.ExplosionAttribute
  ( decodeExplosionAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Type.ExplosionAttribute

decodeExplosionAttributeBits :: DecodeBits ExplosionAttribute
decodeExplosionAttributeBits =
  ExplosionAttribute <$> getBool <*> decodeInt32leBits <*> decodeVectorBits
