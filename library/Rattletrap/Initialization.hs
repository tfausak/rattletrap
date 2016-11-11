module Rattletrap.Initialization where

import Rattletrap.Primitive.Int8Vector
import Rattletrap.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Initialization = Initialization
  { initializationLocation :: Maybe Vector
  , initializationRotation :: Maybe Int8Vector
  } deriving (Eq, Ord, Show)

getInitialization :: Bool -> Bool -> BinaryBit.BitGet Initialization
getInitialization hasLocation hasRotation = do
  location <-
    if hasLocation
      then do
        location <- getVector
        pure (Just location)
      else pure Nothing
  rotation <-
    if hasRotation
      then do
        rotation <- getInt8Vector
        pure (Just rotation)
      else pure Nothing
  pure (Initialization location rotation)

putInitialization :: Initialization -> BinaryBit.BitPut ()
putInitialization initialization = do
  case initializationLocation initialization of
    Nothing -> pure ()
    Just location -> putVector location
  case initializationRotation initialization of
    Nothing -> pure ()
    Just rotation -> putInt8Vector rotation
