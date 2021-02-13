{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ClubColors where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8le.Word8le
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColorsAttribute)

putClubColorsAttribute :: ClubColorsAttribute -> BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBits.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  Word8le.bitPut (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBits.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  Word8le.bitPut (clubColorsAttributeOrangeColor clubColorsAttribute)

decodeClubColorsAttributeBits :: BitGet ClubColorsAttribute
decodeClubColorsAttributeBits =
  ClubColorsAttribute
    <$> getBool
    <*> Word8le.bitGet
    <*> getBool
    <*> Word8le.bitGet
