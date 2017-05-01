module Rattletrap.Attribute.DamageState where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data DamageStateAttribute = DamageStateAttribute
  { damageStateAttributeUnknown1 :: Word8
  , damageStateAttributeUnknown2 :: Bool
  , damageStateAttributeUnknown3 :: Int32
  , damageStateAttributeUnknown4 :: Vector
  , damageStateAttributeUnknown5 :: Bool
  , damageStateAttributeUnknown6 :: Bool
  } deriving (Eq, Ord, Show)

getDamageStateAttribute :: BinaryBit.BitGet DamageStateAttribute
getDamageStateAttribute = do
  unknown1 <- getWord8Bits
  unknown2 <- BinaryBit.getBool
  unknown3 <- getInt32Bits
  unknown4 <- getVector
  unknown5 <- BinaryBit.getBool
  unknown6 <- BinaryBit.getBool
  pure
    (DamageStateAttribute
       unknown1
       unknown2
       unknown3
       unknown4
       unknown5
       unknown6)

putDamageStateAttribute :: DamageStateAttribute -> BinaryBit.BitPut ()
putDamageStateAttribute damageStateAttribute = do
  putWord8Bits (damageStateAttributeUnknown1 damageStateAttribute)
  BinaryBit.putBool (damageStateAttributeUnknown2 damageStateAttribute)
  putInt32Bits (damageStateAttributeUnknown3 damageStateAttribute)
  putVector (damageStateAttributeUnknown4 damageStateAttribute)
  BinaryBit.putBool (damageStateAttributeUnknown5 damageStateAttribute)
  BinaryBit.putBool (damageStateAttributeUnknown6 damageStateAttribute)
