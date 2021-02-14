{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ClubColors where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ClubColorsAttribute = ClubColorsAttribute
  { blueFlag :: Bool
  , blueColor :: Word8le.Word8le
  , orangeFlag :: Bool
  , orangeColor :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColorsAttribute)

bitPut :: ClubColorsAttribute -> BitPut ()
bitPut clubColorsAttribute = do
  BinaryBits.putBool (blueFlag clubColorsAttribute)
  Word8le.bitPut (blueColor clubColorsAttribute)
  BinaryBits.putBool (orangeFlag clubColorsAttribute)
  Word8le.bitPut (orangeColor clubColorsAttribute)

bitGet :: BitGet ClubColorsAttribute
bitGet =
  ClubColorsAttribute
    <$> getBool
    <*> Word8le.bitGet
    <*> getBool
    <*> Word8le.bitGet
