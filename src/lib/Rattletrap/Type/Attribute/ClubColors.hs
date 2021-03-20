module Rattletrap.Type.Attribute.ClubColors where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

data ClubColors = ClubColors
  { blueFlag :: Bool
  , blueColor :: U8.U8
  , orangeFlag :: Bool
  , orangeColor :: U8.U8
  }
  deriving (Eq, Show)

instance Json.FromJSON ClubColors where
  parseJSON = Json.withObject "ClubColors" $ \object -> do
    blueFlag <- Json.required object "blue_flag"
    blueColor <- Json.required object "blue_color"
    orangeFlag <- Json.required object "orange_flag"
    orangeColor <- Json.required object "orange_color"
    pure ClubColors { blueFlag, blueColor, orangeFlag, orangeColor }

instance Json.ToJSON ClubColors where
  toJSON x = Json.object
    [ Json.pair "blue_flag" $ blueFlag x
    , Json.pair "blue_color" $ blueColor x
    , Json.pair "orange_flag" $ orangeFlag x
    , Json.pair "orange_color" $ orangeColor x
    ]

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
bitGet = BitGet.label "ClubColors" $ do
  blueFlag <- BitGet.label "blueFlag" BitGet.bool
  blueColor <- BitGet.label "blueColor" U8.bitGet
  orangeFlag <- BitGet.label "orangeFlag" BitGet.bool
  orangeColor <- BitGet.label "orangeColor" U8.bitGet
  pure ClubColors { blueFlag, blueColor, orangeFlag, orangeColor }
