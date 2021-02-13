{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClubColorsAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8le
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''ClubColorsAttribute)

putClubColorsAttribute :: ClubColorsAttribute -> BinaryBits.BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBits.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBits.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeOrangeColor clubColorsAttribute)
