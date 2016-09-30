module Rattletrap.Initialization where

import Rattletrap.Location
import Rattletrap.Rotation

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Initialization = Initialization
  { initializationLocation :: Maybe Location
  , initializationRotation :: Maybe Rotation
  } deriving (Eq, Ord, Show)

getInitialization :: Bool -> Bool -> BinaryBit.BitGet Initialization
getInitialization hasLocation hasRotation = do
  location <-
    if hasLocation
      then pure Nothing
      else do
        location <- getLocation
        pure (Just location)
  rotation <-
    if hasRotation
      then pure Nothing
      else do
        rotation <- getRotation
        pure (Just rotation)
  pure
    Initialization
    {initializationLocation = location, initializationRotation = rotation}

putInitialization :: Initialization -> BinaryBit.BitPut ()
putInitialization initialization = do
  case initializationLocation initialization of
    Nothing -> pure ()
    Just location -> putLocation location
  case initializationRotation initialization of
    Nothing -> pure ()
    Just rotation -> putRotation rotation
