module Rattletrap.Type.Attribute.Loadouts where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import Rattletrap.Type.Common

data Loadouts = Loadouts
  { blue :: Loadout.Loadout
  , orange :: Loadout.Loadout
  }
  deriving (Eq, Show)

$(deriveJson ''Loadouts)

bitPut :: Loadouts -> BitPut.BitPut
bitPut loadoutsAttribute = Loadout.bitPut (blue loadoutsAttribute)
  <> Loadout.bitPut (orange loadoutsAttribute)

bitGet :: BitGet.BitGet Loadouts
bitGet = Loadouts <$> Loadout.bitGet <*> Loadout.bitGet
