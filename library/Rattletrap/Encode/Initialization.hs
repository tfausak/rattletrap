module Rattletrap.Encode.Initialization
  ( putInitialization
  ) where

import Rattletrap.Type.Initialization
import Rattletrap.Encode.Vector
import Rattletrap.Encode.Int8Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putInitialization :: Initialization -> BinaryBit.BitPut ()
putInitialization initialization = do
  case initializationLocation initialization of
    Nothing -> pure ()
    Just location -> putVector location
  case initializationRotation initialization of
    Nothing -> pure ()
    Just rotation -> putInt8Vector rotation
