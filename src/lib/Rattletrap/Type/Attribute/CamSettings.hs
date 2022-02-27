module Rattletrap.Type.Attribute.CamSettings where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data CamSettings = CamSettings
  { fov :: F32.F32
  , height :: F32.F32
  , angle :: F32.F32
  , distance :: F32.F32
  , stiffness :: F32.F32
  , swivelSpeed :: F32.F32
  , transitionSpeed :: Maybe F32.F32
  }
  deriving (Eq, Show)

instance Argo.HasCodec CamSettings where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ CamSettings
      <$> Argo.required fov "fov"
      <*> Argo.required height "height"
      <*> Argo.required angle "angle"
      <*> Argo.required distance "distance"
      <*> Argo.required stiffness "stiffness"
      <*> Argo.required swivelSpeed "swivel_speed"
      <*> Argo.optional transitionSpeed "transition_speed"

bitPut :: CamSettings -> BitPut.BitPut
bitPut camSettingsAttribute =
  F32.bitPut (fov camSettingsAttribute)
    <> F32.bitPut (height camSettingsAttribute)
    <> F32.bitPut (angle camSettingsAttribute)
    <> F32.bitPut (distance camSettingsAttribute)
    <> F32.bitPut (stiffness camSettingsAttribute)
    <> F32.bitPut (swivelSpeed camSettingsAttribute)
    <> foldMap F32.bitPut (transitionSpeed camSettingsAttribute)

bitGet :: Version.Version -> BitGet.BitGet CamSettings
bitGet version = BitGet.label "CamSettings" $ do
  fov <- BitGet.label "fov" F32.bitGet
  height <- BitGet.label "height" F32.bitGet
  angle <- BitGet.label "angle" F32.bitGet
  distance <- BitGet.label "distance" F32.bitGet
  stiffness <- BitGet.label "stiffness" F32.bitGet
  swivelSpeed <- BitGet.label "swivelSpeed" F32.bitGet
  transitionSpeed <- BitGet.label "transitionSpeed"
    $ Monad.whenMaybe (Version.atLeast 868 20 0 version) F32.bitGet
  pure CamSettings
    { fov
    , height
    , angle
    , distance
    , stiffness
    , swivelSpeed
    , transitionSpeed
    }
