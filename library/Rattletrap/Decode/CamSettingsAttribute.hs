module Rattletrap.Decode.CamSettingsAttribute
  ( decodeCamSettingsAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Type.CamSettingsAttribute
import Rattletrap.Type.Float32le

decodeCamSettingsAttributeBits
  :: (Int, Int, Int) -> DecodeBits CamSettingsAttribute
decodeCamSettingsAttributeBits version =
  CamSettingsAttribute
    <$> getFloat32Bits
    <*> getFloat32Bits
    <*> getFloat32Bits
    <*> getFloat32Bits
    <*> getFloat32Bits
    <*> getFloat32Bits
    <*> getTransitionSpeed version

getTransitionSpeed :: (Int, Int, Int) -> DecodeBits (Maybe Float32le)
getTransitionSpeed version =
  if hasTransitionSpeed version then Just <$> getFloat32Bits else pure Nothing

hasTransitionSpeed :: (Int, Int, Int) -> Bool
hasTransitionSpeed version = version >= (868, 20, 0)
