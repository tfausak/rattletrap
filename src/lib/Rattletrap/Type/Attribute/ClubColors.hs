{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ClubColors where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

data ClubColors = ClubColors
  { blueFlag :: Bool
  , blueColor :: U8.U8
  , orangeFlag :: Bool
  , orangeColor :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColors)

bitPut :: ClubColors -> BitPut.BitPut
bitPut clubColorsAttribute = do
  BitPut.bool (blueFlag clubColorsAttribute)
  U8.bitPut (blueColor clubColorsAttribute)
  BitPut.bool (orangeFlag clubColorsAttribute)
  U8.bitPut (orangeColor clubColorsAttribute)

bitGet :: BitGet ClubColors
bitGet =
  ClubColors
    <$> getBool
    <*> U8.bitGet
    <*> getBool
    <*> U8.bitGet
