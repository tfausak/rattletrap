module Rattletrap.Encode.ReplicationValue
  ( putReplicationValue
  )
where

import Rattletrap.Encode.DestroyedReplication
import Rattletrap.Encode.SpawnedReplication
import Rattletrap.Encode.UpdatedReplication
import Rattletrap.Type.ReplicationValue

import qualified Data.Binary.Bits.Put as BinaryBits

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
