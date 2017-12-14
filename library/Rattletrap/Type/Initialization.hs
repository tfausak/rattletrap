module Rattletrap.Type.Initialization
  ( Initialization(..)
  ) where

import Rattletrap.Type.Vector
import Rattletrap.Type.Int8Vector

data Initialization = Initialization
  { initializationLocation :: Maybe Vector
  -- ^ Not every class has an initial location. See
  -- 'Rattletrap.Data.rawClassesWithLocation'.
  , initializationRotation :: Maybe Int8Vector
  -- ^ Only classes with location can have rotation, but not every one does.
  -- See 'Rattletrap.Data.rawClassesWithRotation'.
  } deriving (Eq, Ord, Show)
