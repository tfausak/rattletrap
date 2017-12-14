module Rattletrap.Encode.AppliedDamageAttribute
  ( putAppliedDamageAttribute
  ) where

import Rattletrap.Type.AppliedDamageAttribute
import Rattletrap.Encode.Word8
import Rattletrap.Encode.Vector
import Rattletrap.Encode.Int32

import qualified Data.Binary.Bits.Put as BinaryBit

putAppliedDamageAttribute :: AppliedDamageAttribute -> BinaryBit.BitPut ()
putAppliedDamageAttribute appliedDamageAttribute = do
  putWord8Bits (appliedDamageAttributeUnknown1 appliedDamageAttribute)
  putVector (appliedDamageAttributeLocation appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown3 appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown4 appliedDamageAttribute)