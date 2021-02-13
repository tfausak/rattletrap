{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CamSettingsAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le

import qualified Data.Binary.Bits.Put as BinaryBits

data CamSettingsAttribute = CamSettingsAttribute
  { camSettingsAttributeFov :: Float32le
  , camSettingsAttributeHeight :: Float32le
  , camSettingsAttributeAngle :: Float32le
  , camSettingsAttributeDistance :: Float32le
  , camSettingsAttributeStiffness :: Float32le
  , camSettingsAttributeSwivelSpeed :: Float32le
  , camSettingsAttributeTransitionSpeed :: Maybe Float32le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''CamSettingsAttribute)

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
