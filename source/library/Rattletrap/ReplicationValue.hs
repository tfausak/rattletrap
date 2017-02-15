module Rattletrap.ReplicationValue
  ( module Rattletrap.ReplicationValue
  , module Rattletrap.Replication.Destroyed
  , module Rattletrap.Replication.Spawned
  , module Rattletrap.Replication.Updated
  ) where

import Rattletrap.ClassAttributeMap
import Rattletrap.Map
import Rattletrap.Primitive
import Rattletrap.Replication.Destroyed
import Rattletrap.Replication.Spawned
import Rattletrap.Replication.Updated

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ReplicationValue
  = SpawnedReplicationValue SpawnedReplication
  -- ^ Creates a new actor.
  | UpdatedReplicationValue UpdatedReplication
  -- ^ Updates an existing actor.
  | DestroyedReplicationValue DestroyedReplication
  -- ^ Destroys an existing actor.
  deriving (Eq, Show)

getReplicationValue
  :: (Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet (ReplicationValue, ActorMap)
getReplicationValue version classAttributeMap actorMap actorId = do
  isOpen <- BinaryBit.getBool
  if isOpen
    then do
      isNew <- BinaryBit.getBool
      if isNew
        then do
          (x, newActorMap) <-
            getSpawnedReplication version classAttributeMap actorMap actorId
          pure (SpawnedReplicationValue x, newActorMap)
        else do
          x <- getUpdatedReplication version classAttributeMap actorMap actorId
          pure (UpdatedReplicationValue x, actorMap)
    else do
      x <- getDestroyedReplication
      pure (DestroyedReplicationValue x, actorMap)

putReplicationValue :: ReplicationValue -> BinaryBit.BitPut ()
putReplicationValue value =
  case value of
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
