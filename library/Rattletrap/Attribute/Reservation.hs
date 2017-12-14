module Rattletrap.Attribute.Reservation where

import Rattletrap.Attribute.UniqueId
import Rattletrap.Type.Text
import Rattletrap.Decode.Text
import Rattletrap.Encode.Text
import Rattletrap.Type.CompressedWord
import Rattletrap.Decode.CompressedWord
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data ReservationAttribute = ReservationAttribute
  { reservationAttributeNumber :: CompressedWord
  , reservationAttributeUniqueId :: UniqueIdAttribute
  , reservationAttributeName :: Maybe Text
  , reservationAttributeUnknown1 :: Bool
  , reservationAttributeUnknown2 :: Bool
  , reservationAttributeUnknown3 :: Maybe Word.Word8
  } deriving (Eq, Ord, Show)

getReservationAttribute :: (Int, Int, Int) -> BinaryBit.BitGet ReservationAttribute
getReservationAttribute version = do
  number <- getCompressedWord 7
  uniqueId <- getUniqueIdAttribute version
  name <- if uniqueIdAttributeSystemId uniqueId == Word8 0
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
