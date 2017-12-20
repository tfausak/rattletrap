module Rattletrap.Decode.CamSettingsAttribute
  ( decodeCamSettingsAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Type.CamSettingsAttribute

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader

decodeCamSettingsAttributeBits
  :: Reader.ReaderT (Int, Int, Int) DecodeBits CamSettingsAttribute
decodeCamSettingsAttributeBits = do
  version <- Reader.ask
  CamSettingsAttribute
    <$> Trans.lift decodeFloat32leBits
    <*> Trans.lift decodeFloat32leBits
    <*> Trans.lift decodeFloat32leBits
    <*> Trans.lift decodeFloat32leBits
    <*> Trans.lift decodeFloat32leBits
    <*> Trans.lift decodeFloat32leBits
    <*> decodeWhen (version >= (868, 20, 0)) (Trans.lift decodeFloat32leBits)
