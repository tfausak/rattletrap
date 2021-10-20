module Rattletrap.Type.Attribute.Loadouts where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import qualified Rattletrap.Utility.Json as Json

data Loadouts = Loadouts
  { blue :: Loadout.Loadout
  , orange :: Loadout.Loadout
  }
  deriving (Eq, Show)

instance Json.FromValue Loadouts where
  fromValue = Json.withObject "Loadouts" $ \object -> do
    blue <- Json.required object "blue"
    orange <- Json.required object "orange"
    pure Loadouts { blue, orange }

instance Json.ToValue Loadouts where
  toValue x =
    Json.object [Json.pair "blue" $ blue x, Json.pair "orange" $ orange x]

schema :: Schema.Schema
schema = Schema.named "attribute-loadouts" $ Schema.object
  [ (Json.pair "blue" $ Schema.ref Loadout.schema, True)
  , (Json.pair "orange" $ Schema.ref Loadout.schema, True)
  ]

bitPut :: Loadouts -> BitPut.BitPut
bitPut loadoutsAttribute = Loadout.bitPut (blue loadoutsAttribute)
  <> Loadout.bitPut (orange loadoutsAttribute)

bitGet :: BitGet.BitGet Loadouts
bitGet = BitGet.label "Loadouts" $ do
  blue <- BitGet.label "blue" Loadout.bitGet
  orange <- BitGet.label "orange" Loadout.bitGet
  pure Loadouts { blue, orange }
