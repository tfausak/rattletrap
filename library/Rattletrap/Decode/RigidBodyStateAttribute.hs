module Rattletrap.Decode.RigidBodyStateAttribute
  ( getRigidBodyStateAttribute
  ) where

import Rattletrap.Decode.CompressedWordVector
import Rattletrap.Decode.Vector
import Rattletrap.Type.RigidBodyStateAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getRigidBodyStateAttribute :: BinaryBit.BitGet RigidBodyStateAttribute
getRigidBodyStateAttribute = do
  sleeping <- BinaryBit.getBool
  location <- getVector
  rotation <- getCompressedWordVector
  linearVelocity <- if sleeping
    then pure Nothing
    else do
      linearVelocity <- getVector
      pure (Just linearVelocity)
  angularVelocity <- if sleeping
    then pure Nothing
    else do
      angularVelocity <- getVector
      pure (Just angularVelocity)
  pure
    ( RigidBodyStateAttribute
      sleeping
      location
      rotation
      linearVelocity
      angularVelocity
    )
