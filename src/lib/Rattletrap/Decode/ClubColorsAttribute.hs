module Rattletrap.Decode.ClubColorsAttribute
  ( decodeClubColorsAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word8le
import Rattletrap.Type.ClubColorsAttribute

decodeClubColorsAttributeBits :: DecodeBits ClubColorsAttribute
decodeClubColorsAttributeBits =
  ClubColorsAttribute
    <$> getBool
    <*> decodeWord8leBits
    <*> getBool
    <*> decodeWord8leBits
