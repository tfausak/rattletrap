module Rattletrap.Encode.CamSettingsAttribute
  ( putCamSettingsAttribute
  ) where

import Rattletrap.Encode.Float32le
import Rattletrap.Type.CamSettingsAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putCamSettingsAttribute :: CamSettingsAttribute -> BinaryBits.BitPut ()
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
