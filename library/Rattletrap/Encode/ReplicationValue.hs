module Rattletrap.Encode.ReplicationValue
  ( putReplicationValue
  ) where

import Rattletrap.Encode.DestroyedReplication
import Rattletrap.Encode.SpawnedReplication
import Rattletrap.Type.ReplicationValue
import Rattletrap.Encode.UpdatedReplication

import qualified Data.Binary.Bits.Put as BinaryBit

putReplicationValue :: ReplicationValue -> BinaryBit.BitPut ()
putReplicationValue value = case value of
  SpawnedReplicationValue x -> do
    BinaryBit.putBool True
    BinaryBit.putBool True
    putSpawnedReplication x
  UpdatedReplicationValue x -> do
    BinaryBit.putBool True
    BinaryBit.putBool False
    putUpdatedReplication x
  DestroyedReplicationValue x -> do
    BinaryBit.putBool False
    putDestroyedReplication x