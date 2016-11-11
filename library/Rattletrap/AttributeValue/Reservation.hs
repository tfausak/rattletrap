module Rattletrap.AttributeValue.Reservation where

import Rattletrap.AttributeValue.UniqueId
import Rattletrap.Primitive.CompressedWord
import Rattletrap.Primitive.Text
import Rattletrap.Primitive.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data ReservationAttributeValue = ReservationAttributeValue
  { reservationAttributeValueNumber :: CompressedWord
  , reservationAttributeValueUniqueId :: UniqueIdAttributeValue
  , reservationAttributeValueName :: Maybe Text
  , reservationAttributeValueUnknown1 :: Bool
  , reservationAttributeValueUnknown2 :: Bool
  , reservationAttributeValueUnknown3 :: Maybe Word.Word8
  } deriving (Eq, Ord, Show)

getReservationAttributeValue :: (Int, Int)
                             -> BinaryBit.BitGet ReservationAttributeValue
getReservationAttributeValue version = do
  number <- getCompressedWord 7
  uniqueId <- getUniqueIdAttributeValue
  name <-
    if uniqueIdAttributeValueSystemId uniqueId == Word8 0
      then pure Nothing
      else do
        name <- getTextBits
        pure (Just name)
  a <- BinaryBit.getBool
  b <- BinaryBit.getBool
  mc <-
    if version < (868, 12)
      then pure Nothing
      else do
        c <- BinaryBit.getWord8 6
        pure (Just c)
  pure (ReservationAttributeValue number uniqueId name a b mc)

putReservationAttributeValue :: ReservationAttributeValue -> BinaryBit.BitPut ()
putReservationAttributeValue reservationAttributeValue = do
  putCompressedWord (reservationAttributeValueNumber reservationAttributeValue)
  putUniqueIdAttributeValue
    (reservationAttributeValueUniqueId reservationAttributeValue)
  case reservationAttributeValueName reservationAttributeValue of
    Nothing -> pure ()
    Just name -> putTextBits name
  BinaryBit.putBool
    (reservationAttributeValueUnknown1 reservationAttributeValue)
  BinaryBit.putBool
    (reservationAttributeValueUnknown2 reservationAttributeValue)
  case reservationAttributeValueUnknown3 reservationAttributeValue of
    Nothing -> pure ()
    Just c -> BinaryBit.putWord8 6 c
