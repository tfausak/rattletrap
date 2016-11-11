module Rattletrap.AttributeValue.CamSettings where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data CamSettingsAttributeValue = CamSettingsAttributeValue
  { camSettingsAttributeValueFov :: Float32
  , camSettingsAttributeValueHeight :: Float32
  , camSettingsAttributeValueAngle :: Float32
  , camSettingsAttributeValueDistance :: Float32
  , camSettingsAttributeValueStiffness :: Float32
  , camSettingsAttributeValueSwivelSpeed :: Float32
  } deriving (Eq, Ord, Show)

getCamSettingsAttributeValue :: BinaryBit.BitGet CamSettingsAttributeValue
getCamSettingsAttributeValue = do
  fov <- getFloat32Bits
  height <- getFloat32Bits
  angle <- getFloat32Bits
  distance <- getFloat32Bits
  stiffness <- getFloat32Bits
  swivelSpeed <- getFloat32Bits
  pure
    (CamSettingsAttributeValue fov height angle distance stiffness swivelSpeed)

putCamSettingsAttributeValue :: CamSettingsAttributeValue -> BinaryBit.BitPut ()
putCamSettingsAttributeValue camSettingsAttributeValue = do
  putFloat32Bits (camSettingsAttributeValueFov camSettingsAttributeValue)
  putFloat32Bits (camSettingsAttributeValueHeight camSettingsAttributeValue)
  putFloat32Bits (camSettingsAttributeValueAngle camSettingsAttributeValue)
  putFloat32Bits (camSettingsAttributeValueDistance camSettingsAttributeValue)
  putFloat32Bits (camSettingsAttributeValueStiffness camSettingsAttributeValue)
  putFloat32Bits
    (camSettingsAttributeValueSwivelSpeed camSettingsAttributeValue)
