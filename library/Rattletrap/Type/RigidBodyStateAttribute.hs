{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.RigidBodyStateAttribute
  ( RigidBodyStateAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector
import Rattletrap.Type.CompressedWordVector

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector
  , rigidBodyStateAttributeRotation :: CompressedWordVector
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON RigidBodyStateAttribute where
  parseJSON = defaultParseJson "RigidBodyStateAttribute"

instance ToJSON RigidBodyStateAttribute where
  toEncoding = defaultToEncoding "RigidBodyStateAttribute"
  toJSON = defaultToJson "RigidBodyStateAttribute"
