module Rattletrap.Encode.ReservationAttribute
  ( putReservationAttribute
  ) where

import Rattletrap.Encode.CompressedWord
import Rattletrap.Encode.Str
import Rattletrap.Encode.UniqueIdAttribute
import Rattletrap.Type.ReservationAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putReservationAttribute :: ReservationAttribute -> BinaryBits.BitPut ()
putReservationAttribute reservationAttribute = do
  putCompressedWord (reservationAttributeNumber reservationAttribute)
  putUniqueIdAttribute (reservationAttributeUniqueId reservationAttribute)
  case reservationAttributeName reservationAttribute of
    Nothing -> pure ()
    Just name -> putTextBits name
  BinaryBits.putBool (reservationAttributeUnknown1 reservationAttribute)
  BinaryBits.putBool (reservationAttributeUnknown2 reservationAttribute)
  case reservationAttributeUnknown3 reservationAttribute of
    Nothing -> pure ()
    Just c -> BinaryBits.putWord8 6 c
