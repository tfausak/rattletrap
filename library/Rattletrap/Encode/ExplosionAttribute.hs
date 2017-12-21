module Rattletrap.Encode.ExplosionAttribute
  ( putExplosionAttribute
  ) where

import Rattletrap.Encode.Int32le
import Rattletrap.Encode.Vector
import Rattletrap.Type.ExplosionAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putExplosionAttribute :: ExplosionAttribute -> BinaryBits.BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBits.putBool False
  putInt32Bits (explosionAttributeActorId explosionAttribute)
  putVector (explosionAttributeLocation explosionAttribute)
