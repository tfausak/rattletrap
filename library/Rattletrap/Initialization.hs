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
      then do
        location <- getLocation
        pure (Just location)
      else pure Nothing
  rotation <-
    if hasRotation
      then do
        rotation <- getRotation
        pure (Just rotation)
      else pure Nothing
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
