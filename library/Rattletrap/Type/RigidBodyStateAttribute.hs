module Rattletrap.Type.RigidBodyStateAttribute
  ( RigidBodyStateAttribute(..)
  ) where

import Rattletrap.Type.Vector
import Rattletrap.Type.CompressedWordVector

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector
  , rigidBodyStateAttributeRotation :: CompressedWordVector
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector
  } deriving (Eq, Ord, Show)
