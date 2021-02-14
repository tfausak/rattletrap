{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CamSettings where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data CamSettingsAttribute = CamSettingsAttribute
  { fov :: Float32le.Float32le
  , height :: Float32le.Float32le
  , angle :: Float32le.Float32le
  , distance :: Float32le.Float32le
  , stiffness :: Float32le.Float32le
  , swivelSpeed :: Float32le.Float32le
  , transitionSpeed :: Maybe Float32le.Float32le
  }
  deriving (Eq, Show)

$(deriveJson ''CamSettingsAttribute)

bitPut :: CamSettingsAttribute -> BitPut ()
bitPut camSettingsAttribute = do
  Float32le.bitPut (fov camSettingsAttribute)
  Float32le.bitPut (height camSettingsAttribute)
  Float32le.bitPut (angle camSettingsAttribute)
  Float32le.bitPut (distance camSettingsAttribute)
  Float32le.bitPut (stiffness camSettingsAttribute)
  Float32le.bitPut (swivelSpeed camSettingsAttribute)
  case transitionSpeed camSettingsAttribute of
    Nothing -> pure ()
    Just transitionSpeed_ -> Float32le.bitPut transitionSpeed_

bitGet
  :: (Int, Int, Int) -> BitGet CamSettingsAttribute
bitGet version =
  CamSettingsAttribute
    <$> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> decodeWhen (version >= (868, 20, 0)) Float32le.bitGet
