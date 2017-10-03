module Rattletrap.Attribute.AppliedDamage where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data AppliedDamageAttribute = AppliedDamageAttribute
  { appliedDamageAttributeUnknown1 :: Word8
  , appliedDamageAttributeLocation :: Vector
  , appliedDamageAttributeUnknown3 :: Int32
  , appliedDamageAttributeUnknown4 :: Int32
  } deriving (Eq, Ord, Show)

getAppliedDamageAttribute :: BinaryBit.BitGet AppliedDamageAttribute
getAppliedDamageAttribute = do
  unknown1 <- getWord8Bits
  location <- getVector
  unknown3 <- getInt32Bits
  unknown4 <- getInt32Bits
  pure (AppliedDamageAttribute unknown1 location unknown3 unknown4)

putAppliedDamageAttribute :: AppliedDamageAttribute -> BinaryBit.BitPut ()
putAppliedDamageAttribute appliedDamageAttribute = do
  putWord8Bits (appliedDamageAttributeUnknown1 appliedDamageAttribute)
  putVector (appliedDamageAttributeLocation appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown3 appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown4 appliedDamageAttribute)
