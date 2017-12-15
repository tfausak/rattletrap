module Rattletrap.Decode.ReservationAttribute
  ( getReservationAttribute
  ) where

import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Decode.UniqueIdAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Decode.Str
import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Get as BinaryBit

getReservationAttribute :: (Int, Int, Int) -> BinaryBit.BitGet ReservationAttribute
getReservationAttribute version = do
  number <- getCompressedWord 7
  uniqueId <- getUniqueIdAttribute version
  name <- if uniqueIdAttributeSystemId uniqueId == Word8le 0
    then pure Nothing
    else do
      name <- getTextBits
      pure (Just name)
  a <- BinaryBit.getBool
  b <- BinaryBit.getBool
  mc <- if version < (868, 12, 0)
    then pure Nothing
    else do
      c <- BinaryBit.getWord8 6
      pure (Just c)
  pure (ReservationAttribute number uniqueId name a b mc)
