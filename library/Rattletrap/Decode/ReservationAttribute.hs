module Rattletrap.Decode.ReservationAttribute
  ( decodeReservationAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.Str
import Rattletrap.Decode.UniqueIdAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Type.Word8le

decodeReservationAttributeBits
  :: (Int, Int, Int) -> DecodeBits ReservationAttribute
decodeReservationAttributeBits version = do
  number <- decodeCompressedWordBits 7
  uniqueId <- decodeUniqueIdAttributeBits version
  ReservationAttribute number uniqueId
    <$> decodeWhen
          (uniqueIdAttributeSystemId uniqueId /= Word8le 0)
          decodeStrBits
    <*> getBool
    <*> getBool
    <*> decodeWhen (version >= (868, 12, 0)) (getWord8Bits 6)
