module Rattletrap.Type.Attribute.Loadouts where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import Rattletrap.Type.Common
import qualified Rattletrap.Utility.Json as Json

data Loadouts = Loadouts
  { blue :: Loadout.Loadout
  , orange :: Loadout.Loadout
  }
  deriving (Eq, Show)

$(deriveJson ''Loadouts)

schema :: Schema.Schema
schema = Schema.named "attribute-loadouts" $ Schema.object
  [ (Json.pair "blue" $ Schema.ref Loadout.schema, True)
  , (Json.pair "orange" $ Schema.ref Loadout.schema, True)
  ]

bitPut :: Loadouts -> BitPut.BitPut
bitPut loadoutsAttribute = Loadout.bitPut (blue loadoutsAttribute)
  <> Loadout.bitPut (orange loadoutsAttribute)

bitGet :: BitGet.BitGet Loadouts
bitGet = Loadouts <$> Loadout.bitGet <*> Loadout.bitGet
