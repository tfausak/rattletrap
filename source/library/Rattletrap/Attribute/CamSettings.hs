module Rattletrap.Attribute.CamSettings where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data CamSettingsAttribute = CamSettingsAttribute
  { camSettingsAttributeFov :: Float32
  , camSettingsAttributeHeight :: Float32
  , camSettingsAttributeAngle :: Float32
  , camSettingsAttributeDistance :: Float32
  , camSettingsAttributeStiffness :: Float32
  , camSettingsAttributeSwivelSpeed :: Float32
  } deriving (Eq, Show)

getCamSettingsAttribute :: BinaryBit.BitGet CamSettingsAttribute
getCamSettingsAttribute = do
  fov <- getFloat32Bits
  height <- getFloat32Bits
  angle <- getFloat32Bits
  distance <- getFloat32Bits
  stiffness <- getFloat32Bits
  swivelSpeed <- getFloat32Bits
  pure (CamSettingsAttribute fov height angle distance stiffness swivelSpeed)

putCamSettingsAttribute :: CamSettingsAttribute -> BinaryBit.BitPut ()
putCamSettingsAttribute camSettingsAttribute = do
  putFloat32Bits (camSettingsAttributeFov camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeHeight camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeAngle camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeDistance camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeStiffness camSettingsAttribute)
  putFloat32Bits (camSettingsAttributeSwivelSpeed camSettingsAttribute)
