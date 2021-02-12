{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ReservationAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.UniqueIdAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

data ReservationAttribute = ReservationAttribute
  { reservationAttributeNumber :: CompressedWord
  , reservationAttributeUniqueId :: UniqueIdAttribute
  , reservationAttributeName :: Maybe Str
  , reservationAttributeUnknown1 :: Bool
  , reservationAttributeUnknown2 :: Bool
  , reservationAttributeUnknown3 :: Maybe Word8
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''ReservationAttribute)

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
