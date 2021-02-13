{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Initialization where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Initialization = Initialization
  { location :: Maybe Vector.Vector
  -- ^ Not every class has an initial location. See
  -- 'Rattletrap.Data.classesWithLocation'.
  , rotation :: Maybe Int8Vector.Int8Vector
  -- ^ Only classes with location can have rotation, but not every one does.
  -- See 'Rattletrap.Data.classesWithRotation'.
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Initialization jsonOptions)

bitPut :: Initialization -> BitPut ()
bitPut initialization = do
  case location initialization of
    Nothing -> pure ()
    Just x -> Vector.bitPut x
  case rotation initialization of
    Nothing -> pure ()
    Just x -> Int8Vector.bitPut x

bitGet
  :: (Int, Int, Int) -> Bool -> Bool -> BitGet Initialization
bitGet version hasLocation hasRotation =
  Initialization
    <$> decodeWhen hasLocation (Vector.bitGet version)
    <*> decodeWhen hasRotation Int8Vector.bitGet
