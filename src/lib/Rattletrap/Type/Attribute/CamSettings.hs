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
      . Argo.fromObjectCodec Argo.Allow
      $ CamSettings
      <$> Argo.project fov (Argo.required (Argo.fromString "fov") Argo.codec)
      <*> Argo.project
            height
            (Argo.required (Argo.fromString "height") Argo.codec)
      <*> Argo.project
            angle
            (Argo.required (Argo.fromString "angle") Argo.codec)
      <*> Argo.project
            distance
            (Argo.required (Argo.fromString "distance") Argo.codec)
      <*> Argo.project
            stiffness
            (Argo.required (Argo.fromString "stiffness") Argo.codec)
      <*> Argo.project
            swivelSpeed
            (Argo.required (Argo.fromString "swivel_speed") Argo.codec)
      <*> Argo.project
            transitionSpeed
            (Argo.optional (Argo.fromString "transition_speed") Argo.codec)

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
