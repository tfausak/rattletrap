module Rattletrap.Encode.AppliedDamageAttribute
  ( putAppliedDamageAttribute
  ) where

import Rattletrap.Type.AppliedDamageAttribute
import Rattletrap.Encode.Word8le
import Rattletrap.Encode.Vector
import Rattletrap.Encode.Int32le

import qualified Data.Binary.Bits.Put as BinaryBit

putAppliedDamageAttribute :: AppliedDamageAttribute -> BinaryBit.BitPut ()
putAppliedDamageAttribute appliedDamageAttribute = do
  putWord8Bits (appliedDamageAttributeUnknown1 appliedDamageAttribute)
  putVector (appliedDamageAttributeLocation appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown3 appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown4 appliedDamageAttribute)
