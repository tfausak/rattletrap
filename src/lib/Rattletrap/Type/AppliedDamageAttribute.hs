{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AppliedDamageAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

data AppliedDamageAttribute = AppliedDamageAttribute
  { appliedDamageAttributeUnknown1 :: Word8le
  , appliedDamageAttributeLocation :: Vector
  , appliedDamageAttributeUnknown3 :: Int32le
  , appliedDamageAttributeUnknown4 :: Int32le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''AppliedDamageAttribute)

putAppliedDamageAttribute :: AppliedDamageAttribute -> BinaryBits.BitPut ()
putAppliedDamageAttribute appliedDamageAttribute = do
  putWord8Bits (appliedDamageAttributeUnknown1 appliedDamageAttribute)
  putVector (appliedDamageAttributeLocation appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown3 appliedDamageAttribute)
  putInt32Bits (appliedDamageAttributeUnknown4 appliedDamageAttribute)
