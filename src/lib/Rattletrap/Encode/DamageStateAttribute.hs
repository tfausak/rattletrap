module Rattletrap.Encode.DamageStateAttribute
  ( putDamageStateAttribute
  ) where

import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Type.Word8le
import Rattletrap.Type.DamageStateAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putDamageStateAttribute :: DamageStateAttribute -> BinaryBits.BitPut ()
putDamageStateAttribute damageStateAttribute = do
  putWord8Bits (damageStateAttributeUnknown1 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown2 damageStateAttribute)
  putInt32Bits (damageStateAttributeUnknown3 damageStateAttribute)
  putVector (damageStateAttributeUnknown4 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown5 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown6 damageStateAttribute)
