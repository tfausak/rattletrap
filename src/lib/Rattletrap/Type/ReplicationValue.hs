{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ReplicationValue where

import Rattletrap.Type.Common
import Rattletrap.Type.DestroyedReplication
import Rattletrap.Type.SpawnedReplication
import Rattletrap.Type.UpdatedReplication

import qualified Data.Binary.Bits.Put as BinaryBits

data ReplicationValue
  = ReplicationValueSpawned SpawnedReplication
  -- ^ Creates a new actor.
  | ReplicationValueUpdated UpdatedReplication
  -- ^ Updates an existing actor.
  | ReplicationValueDestroyed DestroyedReplication
  -- ^ Destroys an existing actor.
  deriving (Eq, Ord, Show)

$(deriveJson ''ReplicationValue)

putReplicationValue :: ReplicationValue -> BinaryBits.BitPut ()
putReplicationValue value = case value of
  ReplicationValueSpawned x -> do
    BinaryBits.putBool True
    BinaryBits.putBool True
    putSpawnedReplication x
  ReplicationValueUpdated x -> do
    BinaryBits.putBool True
    BinaryBits.putBool False
    putUpdatedReplication x
  ReplicationValueDestroyed x -> do
    BinaryBits.putBool False
    putDestroyedReplication x
