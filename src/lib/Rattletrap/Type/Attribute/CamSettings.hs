{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CamSettings where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data CamSettingsAttribute = CamSettingsAttribute
  { camSettingsAttributeFov :: Float32le.Float32le
  , camSettingsAttributeHeight :: Float32le.Float32le
  , camSettingsAttributeAngle :: Float32le.Float32le
  , camSettingsAttributeDistance :: Float32le.Float32le
  , camSettingsAttributeStiffness :: Float32le.Float32le
  , camSettingsAttributeSwivelSpeed :: Float32le.Float32le
  , camSettingsAttributeTransitionSpeed :: Maybe Float32le.Float32le
  }
  deriving (Eq, Show)

$(deriveJson ''CamSettingsAttribute)

putCamSettingsAttribute :: CamSettingsAttribute -> BitPut ()
putCamSettingsAttribute camSettingsAttribute = do
  Float32le.bitPut (camSettingsAttributeFov camSettingsAttribute)
  Float32le.bitPut (camSettingsAttributeHeight camSettingsAttribute)
  Float32le.bitPut (camSettingsAttributeAngle camSettingsAttribute)
  Float32le.bitPut (camSettingsAttributeDistance camSettingsAttribute)
  Float32le.bitPut (camSettingsAttributeStiffness camSettingsAttribute)
  Float32le.bitPut (camSettingsAttributeSwivelSpeed camSettingsAttribute)
  case camSettingsAttributeTransitionSpeed camSettingsAttribute of
    Nothing -> pure ()
    Just transitionSpeed -> Float32le.bitPut transitionSpeed

decodeCamSettingsAttributeBits
  :: (Int, Int, Int) -> BitGet CamSettingsAttribute
decodeCamSettingsAttributeBits version =
  CamSettingsAttribute
    <$> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> Float32le.bitGet
    <*> decodeWhen (version >= (868, 20, 0)) Float32le.bitGet
