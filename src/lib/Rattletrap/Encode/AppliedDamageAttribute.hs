module Rattletrap.Encode.AppliedDamageAttribute
  ( putAppliedDamageAttribute
  ) where

import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Type.Word8le
import Rattletrap.Type.AppliedDamageAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putAppliedDamageAttribute :: AppliedDamageAttribute -> BinaryBits.BitPut ()
putAppliedDamageAttribute appliedDamageAttribute = do
  putWord8Bits (appliedDamageAttributeUnknown1 appliedDamageAttribute)
  putVector (appliedDamageAttributeLocation appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown3 appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown4 appliedDamageAttribute)
