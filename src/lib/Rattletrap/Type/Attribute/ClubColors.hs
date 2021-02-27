module Rattletrap.Type.Attribute.ClubColors where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data ClubColors = ClubColors
  { blueFlag :: Bool
  , blueColor :: U8.U8
  , orangeFlag :: Bool
  , orangeColor :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''ClubColors)

schema :: Schema.Schema
schema = Schema.named "attribute-club-colors" $ Schema.object
  [ (Json.pair "blue_flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "blue_color" $ Schema.ref U8.schema, True)
  , (Json.pair "orange_flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "orange_color" $ Schema.ref U8.schema, True)
  ]

bitPut :: ClubColors -> BitPut.BitPut
bitPut clubColorsAttribute =
  BitPut.bool (blueFlag clubColorsAttribute)
    <> U8.bitPut (blueColor clubColorsAttribute)
    <> BitPut.bool (orangeFlag clubColorsAttribute)
    <> U8.bitPut (orangeColor clubColorsAttribute)

bitGet :: BitGet.BitGet ClubColors
bitGet =
  ClubColors <$> BitGet.bool <*> U8.bitGet <*> BitGet.bool <*> U8.bitGet
