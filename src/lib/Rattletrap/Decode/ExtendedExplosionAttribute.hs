module Rattletrap.Decode.ExtendedExplosionAttribute
  ( decodeExtendedExplosionAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.ExplosionAttribute
import Rattletrap.Decode.FlaggedIntAttribute
import Rattletrap.Type.ExtendedExplosionAttribute

decodeExtendedExplosionAttributeBits
  :: (Int, Int, Int) -> DecodeBits ExtendedExplosionAttribute
decodeExtendedExplosionAttributeBits version =
  ExtendedExplosionAttribute
    <$> decodeExplosionAttributeBits version
    <*> decodeFlaggedIntAttributeBits
