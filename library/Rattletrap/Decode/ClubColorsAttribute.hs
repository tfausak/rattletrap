module Rattletrap.Decode.ClubColorsAttribute
  ( decodeClubColorsAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word8le
import Rattletrap.Type.ClubColorsAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeClubColorsAttributeBits :: DecodeBits ClubColorsAttribute
decodeClubColorsAttributeBits =
  ClubColorsAttribute
    <$> BinaryBits.getBool
    <*> getWord8Bits
    <*> BinaryBits.getBool
    <*> getWord8Bits
