module Rattletrap.Decode.Initialization
  ( getInitialization
  ) where

import Rattletrap.Type.Initialization
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Int8Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getInitialization :: Bool -> Bool -> BinaryBit.BitGet Initialization
getInitialization hasLocation hasRotation = do
  location <- if hasLocation
    then do
      location <- getVector
      pure (Just location)
    else pure Nothing
  rotation <- if hasRotation
    then do
      rotation <- getInt8Vector
      pure (Just rotation)
    else pure Nothing
  pure (Initialization location rotation)
