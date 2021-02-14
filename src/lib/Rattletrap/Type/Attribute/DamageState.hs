{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.DamageState where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DamageStateAttribute = DamageStateAttribute
  { unknown1 :: Word8le.Word8le
  , unknown2 :: Bool
  , unknown3 :: Int32le.Int32le
  , unknown4 :: Vector.Vector
  , unknown5 :: Bool
  , unknown6 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''DamageStateAttribute)

bitPut :: DamageStateAttribute -> BitPut ()
bitPut damageStateAttribute = do
  Word8le.bitPut (unknown1 damageStateAttribute)
  BinaryBits.putBool (unknown2 damageStateAttribute)
  Int32le.bitPut (unknown3 damageStateAttribute)
  Vector.bitPut (unknown4 damageStateAttribute)
  BinaryBits.putBool (unknown5 damageStateAttribute)
  BinaryBits.putBool (unknown6 damageStateAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet DamageStateAttribute
bitGet version =
  DamageStateAttribute
    <$> Word8le.bitGet
    <*> getBool
    <*> Int32le.bitGet
    <*> Vector.bitGet version
    <*> getBool
    <*> getBool
