{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ClubColors where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8le
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColorsAttribute)

putClubColorsAttribute :: ClubColorsAttribute -> BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBits.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBits.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeOrangeColor clubColorsAttribute)

decodeClubColorsAttributeBits :: BitGet ClubColorsAttribute
decodeClubColorsAttributeBits =
  ClubColorsAttribute
    <$> getBool
    <*> decodeWord8leBits
    <*> getBool
    <*> decodeWord8leBits
