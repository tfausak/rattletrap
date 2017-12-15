module Rattletrap.Decode.CamSettingsAttribute
  ( getCamSettingsAttribute
  ) where

import Rattletrap.Type.CamSettingsAttribute
import Rattletrap.Decode.Float32le

import qualified Data.Binary.Bits.Get as BinaryBit

getCamSettingsAttribute
  :: (Int, Int, Int) -> BinaryBit.BitGet CamSettingsAttribute
getCamSettingsAttribute version = do
  fov <- getFloat32Bits
  height <- getFloat32Bits
  angle <- getFloat32Bits
  distance <- getFloat32Bits
  stiffness <- getFloat32Bits
  swivelSpeed <- getFloat32Bits
  transitionSpeed <- if version >= (868, 20, 0)
    then do
      x <- getFloat32Bits
      pure (Just x)
    else pure Nothing
  pure
    ( CamSettingsAttribute
      fov
      height
      angle
      distance
      stiffness
      swivelSpeed
      transitionSpeed
    )
