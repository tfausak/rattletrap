module Rattletrap.Encode.ClubColorsAttribute
  ( putClubColorsAttribute
  ) where

import Rattletrap.Type.Word8le
import Rattletrap.Type.ClubColorsAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putClubColorsAttribute :: ClubColorsAttribute -> BinaryBits.BitPut ()
putClubColorsAttribute clubColorsAttribute = do
  BinaryBits.putBool (clubColorsAttributeBlueFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeBlueColor clubColorsAttribute)
  BinaryBits.putBool (clubColorsAttributeOrangeFlag clubColorsAttribute)
  putWord8Bits (clubColorsAttributeOrangeColor clubColorsAttribute)
