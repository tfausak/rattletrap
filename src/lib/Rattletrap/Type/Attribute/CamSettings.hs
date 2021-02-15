{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CamSettings where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import Rattletrap.Utility.Monad

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

$(deriveJson ''CamSettings)

bitPut :: CamSettings -> BitPut.BitPut
bitPut camSettingsAttribute =
  F32.bitPut (fov camSettingsAttribute)
    <> F32.bitPut (height camSettingsAttribute)
    <> F32.bitPut (angle camSettingsAttribute)
    <> F32.bitPut (distance camSettingsAttribute)
    <> F32.bitPut (stiffness camSettingsAttribute)
    <> F32.bitPut (swivelSpeed camSettingsAttribute)
    <> foldMap F32.bitPut (transitionSpeed camSettingsAttribute)

bitGet :: (Int, Int, Int) -> BitGet.BitGet CamSettings
bitGet version =
  CamSettings
    <$> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> whenMaybe (version >= (868, 20, 0)) F32.bitGet
