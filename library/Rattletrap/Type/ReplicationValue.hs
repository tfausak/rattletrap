{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ReplicationValue
  ( ReplicationValue(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.DestroyedReplication
import Rattletrap.Type.SpawnedReplication
import Rattletrap.Type.UpdatedReplication

data ReplicationValue
  = SpawnedReplicationValue SpawnedReplication
  -- ^ Creates a new actor.
  | UpdatedReplicationValue UpdatedReplication
  -- ^ Updates an existing actor.
  | DestroyedReplicationValue DestroyedReplication
  -- ^ Destroys an existing actor.
  deriving (Eq, Generic, Ord, Show)

instance FromJSON ReplicationValue where
  parseJSON = defaultParseJson "ReplicationValue"

instance ToJSON ReplicationValue where
  toEncoding = defaultToEncoding "ReplicationValue"
  toJSON = defaultToJson "ReplicationValue"
