{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CamSettings where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data CamSettingsAttribute = CamSettingsAttribute
  { fov :: F32.F32
  , height :: F32.F32
  , angle :: F32.F32
  , distance :: F32.F32
  , stiffness :: F32.F32
  , swivelSpeed :: F32.F32
  , transitionSpeed :: Maybe F32.F32
  }
  deriving (Eq, Show)

$(deriveJson ''CamSettingsAttribute)

bitPut :: CamSettingsAttribute -> BitPut ()
bitPut camSettingsAttribute = do
  F32.bitPut (fov camSettingsAttribute)
  F32.bitPut (height camSettingsAttribute)
  F32.bitPut (angle camSettingsAttribute)
  F32.bitPut (distance camSettingsAttribute)
  F32.bitPut (stiffness camSettingsAttribute)
  F32.bitPut (swivelSpeed camSettingsAttribute)
  case transitionSpeed camSettingsAttribute of
    Nothing -> pure ()
    Just transitionSpeed_ -> F32.bitPut transitionSpeed_

bitGet
  :: (Int, Int, Int) -> BitGet CamSettingsAttribute
bitGet version =
  CamSettingsAttribute
    <$> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> F32.bitGet
    <*> decodeWhen (version >= (868, 20, 0)) F32.bitGet
