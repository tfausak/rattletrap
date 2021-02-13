{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CamSettings where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data CamSettingsAttribute = CamSettingsAttribute
  { camSettingsAttributeFov :: Float32le
  , camSettingsAttributeHeight :: Float32le
  , camSettingsAttributeAngle :: Float32le
  , camSettingsAttributeDistance :: Float32le
  , camSettingsAttributeStiffness :: Float32le
  , camSettingsAttributeSwivelSpeed :: Float32le
  , camSettingsAttributeTransitionSpeed :: Maybe Float32le
  }
  deriving (Eq, Show)

$(deriveJson ''CamSettingsAttribute)

putCamSettingsAttribute :: CamSettingsAttribute -> BitPut ()
putCamSettingsAttribute camSettingsAttribute = do
  putFloat32Bits (camSettingsAttributeFov camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeHeight camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeAngle camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeDistance camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeStiffness camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeSwivelSpeed camSettingsAttribute)
  case camSettingsAttributeTransitionSpeed camSettingsAttribute of
    Nothing -> pure ()
    Just transitionSpeed -> putFloat32Bits transitionSpeed

decodeCamSettingsAttributeBits
  :: (Int, Int, Int) -> BitGet CamSettingsAttribute
decodeCamSettingsAttributeBits version =
  CamSettingsAttribute
    <$> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeWhen (version >= (868, 20, 0)) decodeFloat32leBits
