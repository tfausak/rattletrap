module Rattletrap.Type.Attribute.LoadoutsOnline where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.LoadoutOnline as LoadoutOnline
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data LoadoutsOnline = LoadoutsOnline
  { blue :: LoadoutOnline.LoadoutOnline
  , orange :: LoadoutOnline.LoadoutOnline
  , unknown1 :: Bool
  , unknown2 :: Bool
  }
  deriving (Eq, Show)

instance Json.FromJSON LoadoutsOnline where
  parseJSON = Json.withObject "LoadoutsOnline" $ \object -> do
    blue <- Json.required object "blue"
    orange <- Json.required object "orange"
    unknown1 <- Json.required object "unknown1"
    unknown2 <- Json.required object "unknown2"
    pure LoadoutsOnline { blue, orange, unknown1, unknown2 }

instance Json.ToJSON LoadoutsOnline where
  toJSON x = Json.object
    [ Json.pair "blue" $ blue x
    , Json.pair "orange" $ orange x
    , Json.pair "unknown1" $ unknown1 x
    , Json.pair "unknown2" $ unknown2 x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-loadouts-online" $ Schema.object
  [ (Json.pair "blue" $ Schema.ref LoadoutOnline.schema, True)
  , (Json.pair "orange" $ Schema.ref LoadoutOnline.schema, True)
  , (Json.pair "unknown1" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown2" $ Schema.ref Schema.boolean, True)
  ]

bitPut :: LoadoutsOnline -> BitPut.BitPut
bitPut loadoutsOnlineAttribute =
  LoadoutOnline.bitPut (blue loadoutsOnlineAttribute)
    <> LoadoutOnline.bitPut (orange loadoutsOnlineAttribute)
    <> BitPut.bool (unknown1 loadoutsOnlineAttribute)
    <> BitPut.bool (unknown2 loadoutsOnlineAttribute)

bitGet
  :: Version.Version -> Map.Map U32.U32 Str.Str -> BitGet.BitGet LoadoutsOnline
bitGet version objectMap = do
  blue <- LoadoutOnline.bitGet version objectMap
  orange <- LoadoutOnline.bitGet version objectMap
  unknown1 <- BitGet.bool
  unknown2 <- BitGet.bool
  pure LoadoutsOnline { blue, orange, unknown1, unknown2 }
