{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.AppliedDamage where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data AppliedDamageAttribute = AppliedDamageAttribute
  { unknown1 :: Word8le.Word8le
  , location :: Vector.Vector
  , unknown3 :: Int32le.Int32le
  , unknown4 :: Int32le.Int32le
  }
  deriving (Eq, Show)

$(deriveJsonWith ''AppliedDamageAttribute jsonOptions)

bitPut :: AppliedDamageAttribute -> BitPut ()
bitPut appliedDamageAttribute = do
  Word8le.bitPut (unknown1 appliedDamageAttribute)
  Vector.bitPut (location appliedDamageAttribute)
  Int32le.bitPut (unknown3 appliedDamageAttribute)
  Int32le.bitPut (unknown4 appliedDamageAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet AppliedDamageAttribute
bitGet version =
  AppliedDamageAttribute
    <$> Word8le.bitGet
    <*> Vector.bitGet version
    <*> Int32le.bitGet
    <*> Int32le.bitGet
