{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ReplicationValue
  ( ReplicationValue(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.DestroyedReplication
import Rattletrap.Type.SpawnedReplication
import Rattletrap.Type.UpdatedReplication

data ReplicationValue
  = ReplicationValueSpawned SpawnedReplication
  -- ^ Creates a new actor.
  | ReplicationValueUpdated UpdatedReplication
  -- ^ Updates an existing actor.
  | ReplicationValueDestroyed DestroyedReplication
  -- ^ Destroys an existing actor.
  deriving (Eq, Generic, Ord, Show)

instance FromJSON ReplicationValue where
  parseJSON = defaultParseJson "ReplicationValue"

instance ToJSON ReplicationValue where
  toEncoding = defaultToEncoding "ReplicationValue"
  toJSON = defaultToJson "ReplicationValue"
