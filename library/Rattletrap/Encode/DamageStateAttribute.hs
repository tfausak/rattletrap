module Rattletrap.Encode.DamageStateAttribute
  ( putDamageStateAttribute
  ) where

import Rattletrap.Type.DamageStateAttribute
import Rattletrap.Encode.Word8
import Rattletrap.Encode.Int32
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putDamageStateAttribute :: DamageStateAttribute -> BinaryBit.BitPut ()
putDamageStateAttribute damageStateAttribute = do
  putWord8Bits (damageStateAttributeUnknown1 damageStateAttribute)
  BinaryBit.putBool (damageStateAttributeUnknown2 damageStateAttribute)
  putInt32Bits (damageStateAttributeUnknown3 damageStateAttribute)
  putVector (damageStateAttributeUnknown4 damageStateAttribute)
  BinaryBit.putBool (damageStateAttributeUnknown5 damageStateAttribute)
  BinaryBit.putBool (damageStateAttributeUnknown6 damageStateAttribute)
