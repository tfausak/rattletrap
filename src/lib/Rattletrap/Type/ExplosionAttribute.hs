{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ExplosionAttribute
  ( ExplosionAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeFlag :: Bool
  , explosionAttributeActorId :: Int32le
  , explosionAttributeLocation :: Vector
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''ExplosionAttribute)
