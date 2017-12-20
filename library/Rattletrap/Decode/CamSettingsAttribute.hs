module Rattletrap.Decode.CamSettingsAttribute
  ( decodeCamSettingsAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Type.CamSettingsAttribute

decodeCamSettingsAttributeBits
  :: (Int, Int, Int) -> DecodeBits CamSettingsAttribute
decodeCamSettingsAttributeBits version =
  CamSettingsAttribute
    <$> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeFloat32leBits
    <*> decodeWhen (version >= (868, 20, 0)) decodeFloat32leBits
