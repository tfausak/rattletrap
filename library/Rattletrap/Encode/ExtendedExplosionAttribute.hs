module Rattletrap.Encode.ExtendedExplosionAttribute
  ( putExtendedExplosionAttribute
  ) where

import Rattletrap.Type.ExtendedExplosionAttribute
import Rattletrap.Encode.Int32
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putExtendedExplosionAttribute
  :: ExtendedExplosionAttribute -> BinaryBit.BitPut ()
putExtendedExplosionAttribute extendedExplosionAttribute = do
  BinaryBit.putBool False
  putInt32Bits (extendedExplosionAttributeActorId extendedExplosionAttribute)
  putVector (extendedExplosionAttributeLocation extendedExplosionAttribute)
  BinaryBit.putBool
    (extendedExplosionAttributeUnknown1 extendedExplosionAttribute)
  putInt32Bits (extendedExplosionAttributeUnknown2 extendedExplosionAttribute)
