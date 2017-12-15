module Rattletrap.Encode.ClubColorsAttribute
  ( putClubColorsAttribute
  ) where

import Rattletrap.Encode.Word8le
import Rattletrap.Type.ClubColorsAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putClubColorsAttribute :: ClubColorsAttribute -> BinaryBit.BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBit.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBit.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeOrangeColor clubColorsAttribute)
