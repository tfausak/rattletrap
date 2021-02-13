{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Initialization where

import Rattletrap.Type.Common
import Rattletrap.Type.Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Initialization = Initialization
  { initializationLocation :: Maybe Vector.Vector
  -- ^ Not every class has an initial location. See
  -- 'Rattletrap.Data.classesWithLocation'.
  , initializationRotation :: Maybe Int8Vector
  -- ^ Only classes with location can have rotation, but not every one does.
  -- See 'Rattletrap.Data.classesWithRotation'.
  }
  deriving (Eq, Show)

$(deriveJson ''Initialization)

putInitialization :: Initialization -> BitPut ()
putInitialization initialization = do
  case initializationLocation initialization of
    Nothing -> pure ()
    Just location -> Vector.bitPut location
  case initializationRotation initialization of
    Nothing -> pure ()
    Just rotation -> putInt8Vector rotation

decodeInitializationBits
  :: (Int, Int, Int) -> Bool -> Bool -> BitGet Initialization
decodeInitializationBits version hasLocation hasRotation =
  Initialization
    <$> decodeWhen hasLocation (Vector.bitGet version)
    <*> decodeWhen hasRotation decodeInt8VectorBits
