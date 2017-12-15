module Rattletrap.Encode.ReservationAttribute
  ( putReservationAttribute
  ) where

import Rattletrap.Encode.UniqueIdAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Encode.Str
import Rattletrap.Encode.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBit

putReservationAttribute :: ReservationAttribute -> BinaryBit.BitPut ()
putReservationAttribute reservationAttribute = do
  putCompressedWord (reservationAttributeNumber reservationAttribute)
  putUniqueIdAttribute (reservationAttributeUniqueId reservationAttribute)
  case reservationAttributeName reservationAttribute of
    Nothing -> pure ()
    Just name -> putTextBits name
  BinaryBit.putBool (reservationAttributeUnknown1 reservationAttribute)
  BinaryBit.putBool (reservationAttributeUnknown2 reservationAttribute)
  case reservationAttributeUnknown3 reservationAttribute of
    Nothing -> pure ()
    Just c -> BinaryBit.putWord8 6 c
