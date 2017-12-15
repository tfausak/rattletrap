module Rattletrap.Decode.ClubColorsAttribute
  ( getClubColorsAttribute
  ) where

import Rattletrap.Type.ClubColorsAttribute
import Rattletrap.Decode.Word8le

import qualified Data.Binary.Bits.Get as BinaryBit

getClubColorsAttribute :: BinaryBit.BitGet ClubColorsAttribute
getClubColorsAttribute = do
  blueFlag <- BinaryBit.getBool
  blueColor <- getWord8Bits
  orangeFlag <- BinaryBit.getBool
  orangeColor <- getWord8Bits
  pure (ClubColorsAttribute blueFlag blueColor orangeFlag orangeColor)
