module Rattletrap.Decode.ExtendedExplosionAttribute
  ( getExtendedExplosionAttribute
  ) where

import Rattletrap.Decode.ExplosionAttribute
import Rattletrap.Decode.Int32le
import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Type.ExtendedExplosionAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getExtendedExplosionAttribute :: BinaryBit.BitGet ExtendedExplosionAttribute
getExtendedExplosionAttribute = do
  x <- decodeExplosionAttributeBits
  unknown1 <- BinaryBit.getBool
  unknown2 <- getInt32Bits
  pure
    ( ExtendedExplosionAttribute
      (explosionAttributeActorId x)
      (explosionAttributeLocation x)
      unknown1
      unknown2
    )
