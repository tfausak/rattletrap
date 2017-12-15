module Rattletrap.Decode.ExtendedExplosionAttribute
  ( getExtendedExplosionAttribute
  ) where

import Rattletrap.Type.ExtendedExplosionAttribute
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getExtendedExplosionAttribute :: BinaryBit.BitGet ExtendedExplosionAttribute
getExtendedExplosionAttribute = do
  False <- BinaryBit.getBool
  actorId <- getInt32Bits
  location <- getVector
  unknown1 <- BinaryBit.getBool
  unknown2 <- getInt32Bits
  pure (ExtendedExplosionAttribute actorId location unknown1 unknown2)
