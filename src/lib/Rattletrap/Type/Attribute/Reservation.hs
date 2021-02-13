{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Reservation where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import Rattletrap.Type.Attribute.UniqueId
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ReservationAttribute = ReservationAttribute
  { reservationAttributeNumber :: CompressedWord.CompressedWord
  , reservationAttributeUniqueId :: UniqueIdAttribute
  , reservationAttributeName :: Maybe Str.Str
  , reservationAttributeUnknown1 :: Bool
  , reservationAttributeUnknown2 :: Bool
  , reservationAttributeUnknown3 :: Maybe Word8
  }
  deriving (Eq, Show)

$(deriveJson ''ReservationAttribute)

putReservationAttribute :: ReservationAttribute -> BitPut ()
putReservationAttribute reservationAttribute = do
  CompressedWord.bitPut (reservationAttributeNumber reservationAttribute)
  putUniqueIdAttribute (reservationAttributeUniqueId reservationAttribute)
  case reservationAttributeName reservationAttribute of
    Nothing -> pure ()
    Just name -> Str.bitPut name
  BinaryBits.putBool (reservationAttributeUnknown1 reservationAttribute)
  BinaryBits.putBool (reservationAttributeUnknown2 reservationAttribute)
  case reservationAttributeUnknown3 reservationAttribute of
    Nothing -> pure ()
    Just c -> BinaryBits.putWord8 6 c

decodeReservationAttributeBits
  :: (Int, Int, Int) -> BitGet ReservationAttribute
decodeReservationAttributeBits version = do
  number <- CompressedWord.bitGet 7
  uniqueId <- decodeUniqueIdAttributeBits version
  ReservationAttribute number uniqueId
    <$> decodeWhen
          (uniqueIdAttributeSystemId uniqueId /= Word8le.fromWord8 0)
          Str.bitGet
    <*> getBool
    <*> getBool
    <*> decodeWhen (version >= (868, 12, 0)) (getWord8Bits 6)
