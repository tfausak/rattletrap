module Rattletrap.Attribute.AppliedDamage where

import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.Encode.Word8
import Rattletrap.Type.Vector
import Rattletrap.Decode.Vector
import Rattletrap.Encode.Vector
import Rattletrap.Type.Int32
import Rattletrap.Decode.Int32
import Rattletrap.Encode.Int32

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
