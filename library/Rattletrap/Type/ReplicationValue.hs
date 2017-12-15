{-# LANGUAGE TemplateHaskell #-}

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
  deriving (Eq, Ord, Show)

$(deriveJson ''ReplicationValue)
