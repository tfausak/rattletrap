{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ClubColors where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ClubColorsAttribute = ClubColorsAttribute
  { blueFlag :: Bool
  , blueColor :: U8.U8
  , orangeFlag :: Bool
  , orangeColor :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColorsAttribute)

bitPut :: ClubColorsAttribute -> BitPut ()
bitPut clubColorsAttribute = do
  BinaryBits.putBool (blueFlag clubColorsAttribute)
  U8.bitPut (blueColor clubColorsAttribute)
  BinaryBits.putBool (orangeFlag clubColorsAttribute)
  U8.bitPut (orangeColor clubColorsAttribute)

bitGet :: BitGet ClubColorsAttribute
bitGet =
  ClubColorsAttribute
    <$> getBool
    <*> U8.bitGet
    <*> getBool
    <*> U8.bitGet
