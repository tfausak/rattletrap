module Rattletrap.Encode.ExplosionAttribute
  ( putExplosionAttribute
  ) where

import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Encode.Int32
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putExplosionAttribute :: ExplosionAttribute -> BinaryBit.BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBit.putBool False
  putInt32Bits (explosionAttributeActorId explosionAttribute)
  putVector (explosionAttributeLocation explosionAttribute)
