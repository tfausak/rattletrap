module Rattletrap.Type.Attribute.LoadoutsOnline where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.LoadoutOnline as LoadoutOnline
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data LoadoutsOnline = LoadoutsOnline
  { blue :: LoadoutOnline.LoadoutOnline
  , orange :: LoadoutOnline.LoadoutOnline
  , unknown1 :: Bool
  , unknown2 :: Bool
  }
  deriving (Eq, Show)

instance Argo.HasCodec LoadoutsOnline where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ LoadoutsOnline
      <$> Argo.required blue "blue"
      <*> Argo.required orange "orange"
      <*> Argo.required unknown1 "unknown1"
      <*> Argo.required unknown2 "unknown2"

bitPut :: LoadoutsOnline -> BitPut.BitPut
bitPut loadoutsOnlineAttribute =
  LoadoutOnline.bitPut (blue loadoutsOnlineAttribute)
    <> LoadoutOnline.bitPut (orange loadoutsOnlineAttribute)
    <> BitPut.bool (unknown1 loadoutsOnlineAttribute)
    <> BitPut.bool (unknown2 loadoutsOnlineAttribute)

bitGet
  :: Version.Version -> Map.Map U32.U32 Str.Str -> BitGet.BitGet LoadoutsOnline
bitGet version objectMap = BitGet.label "LoadoutsOnline" $ do
  blue <- BitGet.label "blue" $ LoadoutOnline.bitGet version objectMap
  orange <- BitGet.label "orange" $ LoadoutOnline.bitGet version objectMap
  unknown1 <- BitGet.label "unknown1" BitGet.bool
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  pure LoadoutsOnline { blue, orange, unknown1, unknown2 }
