{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.DamageState where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DamageStateAttribute = DamageStateAttribute
  { damageStateAttributeUnknown1 :: Word8le.Word8le
  , damageStateAttributeUnknown2 :: Bool
  , damageStateAttributeUnknown3 :: Int32le
  , damageStateAttributeUnknown4 :: Vector.Vector
  , damageStateAttributeUnknown5 :: Bool
  , damageStateAttributeUnknown6 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''DamageStateAttribute)

putDamageStateAttribute :: DamageStateAttribute -> BitPut ()
putDamageStateAttribute damageStateAttribute = do
  Word8le.bitPut (damageStateAttributeUnknown1 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown2 damageStateAttribute)
  putInt32Bits (damageStateAttributeUnknown3 damageStateAttribute)
  Vector.bitPut (damageStateAttributeUnknown4 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown5 damageStateAttribute)
  BinaryBits.putBool (damageStateAttributeUnknown6 damageStateAttribute)

decodeDamageStateAttributeBits
  :: (Int, Int, Int) -> BitGet DamageStateAttribute
decodeDamageStateAttributeBits version =
  DamageStateAttribute
    <$> Word8le.bitGet
    <*> getBool
    <*> decodeInt32leBits
    <*> Vector.bitGet version
    <*> getBool
    <*> getBool
