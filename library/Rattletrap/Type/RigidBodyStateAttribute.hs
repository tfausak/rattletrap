{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RigidBodyStateAttribute
  ( RigidBodyStateAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWordVector
import Rattletrap.Type.Vector

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector
  , rigidBodyStateAttributeRotation :: CompressedWordVector
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector
  } deriving (Eq, Ord, Show)

$(deriveJson ''RigidBodyStateAttribute)
