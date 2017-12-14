module Rattletrap.Type.ExplosionAttribute
  ( ExplosionAttribute(..)
  ) where

import Rattletrap.Type.Int32
import Rattletrap.Type.Vector

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeActorId :: Int32
  , explosionAttributeLocation :: Vector
  } deriving (Eq, Ord, Show)
