module Rattletrap.Type.Attribute.ClubColors where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8

data ClubColors = ClubColors
  { blueFlag :: Bool
  , blueColor :: U8.U8
  , orangeFlag :: Bool
  , orangeColor :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColors)

bitPut :: ClubColors -> BitPut.BitPut
bitPut clubColorsAttribute =
  BitPut.bool (blueFlag clubColorsAttribute)
    <> U8.bitPut (blueColor clubColorsAttribute)
    <> BitPut.bool (orangeFlag clubColorsAttribute)
    <> U8.bitPut (orangeColor clubColorsAttribute)

bitGet :: BitGet.BitGet ClubColors
bitGet =
  ClubColors <$> BitGet.bool <*> U8.bitGet <*> BitGet.bool <*> U8.bitGet
