module Rattletrap.Attribute.ClubColors where

import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.Encode.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8
  } deriving (Eq, Ord, Show)

getClubColorsAttribute :: BinaryBit.BitGet ClubColorsAttribute
getClubColorsAttribute = do
  blueFlag <- BinaryBit.getBool
  blueColor <- getWord8Bits
  orangeFlag <- BinaryBit.getBool
  orangeColor <- getWord8Bits
  pure (ClubColorsAttribute blueFlag blueColor orangeFlag orangeColor)

putClubColorsAttribute :: ClubColorsAttribute -> BinaryBit.BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBit.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBit.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeOrangeColor clubColorsAttribute)
