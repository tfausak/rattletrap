module Rattletrap.Encode.CamSettingsAttribute
  ( putCamSettingsAttribute
  ) where

import Rattletrap.Type.CamSettingsAttribute
import Rattletrap.Encode.Float32le

import qualified Data.Binary.Bits.Put as BinaryBit

putCamSettingsAttribute :: CamSettingsAttribute -> BinaryBit.BitPut ()
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
