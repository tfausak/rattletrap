module Rattletrap.AttributeValue.ClubColors where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ClubColorsAttributeValue = ClubColorsAttributeValue
  { clubColorsAttributeValueBlueFlag :: Bool
  , clubColorsAttributeValueBlueColor :: Word8
  , clubColorsAttributeValueOrangeFlag :: Bool
  , clubColorsAttributeValueOrangeColor :: Word8
  } deriving (Eq, Ord, Show)

getClubColorsAttributeValue :: BinaryBit.BitGet ClubColorsAttributeValue
getClubColorsAttributeValue = do
  blueFlag <- BinaryBit.getBool
  blueColor <- getWord8Bits
  orangeFlag <- BinaryBit.getBool
  orangeColor <- getWord8Bits
  pure (ClubColorsAttributeValue blueFlag blueColor orangeFlag orangeColor)

putClubColorsAttributeValue :: ClubColorsAttributeValue -> BinaryBit.BitPut ()
putClubColorsAttributeValue clubColorsAttributeValue = do
  BinaryBit.putBool (clubColorsAttributeValueBlueFlag clubColorsAttributeValue)
  putWord8Bits (clubColorsAttributeValueBlueColor clubColorsAttributeValue)
  BinaryBit.putBool
    (clubColorsAttributeValueOrangeFlag clubColorsAttributeValue)
  putWord8Bits (clubColorsAttributeValueOrangeColor clubColorsAttributeValue)
