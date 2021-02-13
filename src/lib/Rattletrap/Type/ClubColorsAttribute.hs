{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClubColorsAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8le
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColorsAttribute)

putClubColorsAttribute :: ClubColorsAttribute -> BinaryBits.BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBits.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBits.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeOrangeColor clubColorsAttribute)

decodeClubColorsAttributeBits :: DecodeBits ClubColorsAttribute
decodeClubColorsAttributeBits =
  ClubColorsAttribute
    <$> getBool
    <*> decodeWord8leBits
    <*> getBool
    <*> decodeWord8leBits
