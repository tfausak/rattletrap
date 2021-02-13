{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Reservation where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.Attribute.UniqueId
import Rattletrap.Decode.Common
import Rattletrap.Type.Word8le
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ReservationAttribute = ReservationAttribute
  { reservationAttributeNumber :: CompressedWord
  , reservationAttributeUniqueId :: UniqueIdAttribute
  , reservationAttributeName :: Maybe Str
  , reservationAttributeUnknown1 :: Bool
  , reservationAttributeUnknown2 :: Bool
  , reservationAttributeUnknown3 :: Maybe Word8
  }
  deriving (Eq, Show)

$(deriveJson ''ReservationAttribute)

putReservationAttribute :: ReservationAttribute -> BitPut ()
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

decodeReservationAttributeBits
  :: (Int, Int, Int) -> BitGet ReservationAttribute
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
