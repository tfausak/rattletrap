{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.DamageStateAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DamageStateAttribute = DamageStateAttribute
  { damageStateAttributeUnknown1 :: Word8le
  , damageStateAttributeUnknown2 :: Bool
  , damageStateAttributeUnknown3 :: Int32le
  , damageStateAttributeUnknown4 :: Vector
  , damageStateAttributeUnknown5 :: Bool
  , damageStateAttributeUnknown6 :: Bool
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''DamageStateAttribute)

putDamageStateAttribute :: DamageStateAttribute -> BinaryBits.BitPut ()
putDamageStateAttribute damageStateAttribute = do
  putWord8Bits (damageStateAttributeUnknown1 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown2 damageStateAttribute)
  putInt32Bits (damageStateAttributeUnknown3 damageStateAttribute)
  putVector (damageStateAttributeUnknown4 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown5 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown6 damageStateAttribute)

decodeDamageStateAttributeBits
  :: (Int, Int, Int) -> DecodeBits DamageStateAttribute
decodeDamageStateAttributeBits version =
  DamageStateAttribute
    <$> decodeWord8leBits
    <*> getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits version
    <*> getBool
    <*> getBool
