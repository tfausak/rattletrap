module Rattletrap.Type.Attribute.Loadouts where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import qualified Rattletrap.Vendor.Argo as Argo

data Loadouts = Loadouts
  { blue :: Loadout.Loadout
  , orange :: Loadout.Loadout
  }
  deriving (Eq, Show)

instance Argo.HasCodec Loadouts where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Loadouts
      <$> Argo.required blue "blue"
      <*> Argo.required orange "orange"

bitPut :: Loadouts -> BitPut.BitPut
bitPut loadoutsAttribute = Loadout.bitPut (blue loadoutsAttribute)
  <> Loadout.bitPut (orange loadoutsAttribute)

bitGet :: BitGet.BitGet Loadouts
bitGet = BitGet.label "Loadouts" $ do
  blue <- BitGet.label "blue" Loadout.bitGet
  orange <- BitGet.label "orange" Loadout.bitGet
  pure Loadouts { blue, orange }
