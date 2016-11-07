module Rattletrap.ReplicationValue
  ( module Rattletrap.ReplicationValue
  , module Rattletrap.ReplicationValue.Destroyed
  , module Rattletrap.ReplicationValue.Spawned
  , module Rattletrap.ReplicationValue.Updated
  ) where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.CompressedWord
import Rattletrap.ReplicationValue.Destroyed
import Rattletrap.ReplicationValue.Spawned
import Rattletrap.ReplicationValue.Updated

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ReplicationValue
  = SpawnedReplication SpawnedReplicationValue
  | UpdatedReplication UpdatedReplicationValue
  | DestroyedReplication DestroyedReplicationValue
  deriving (Eq, Ord, Show)

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
            getSpawnedReplicationValue classAttributeMap actorMap actorId
          pure (SpawnedReplication x, newActorMap)
        else do
          x <-
            getUpdatedReplicationValue
              version
              classAttributeMap
              actorMap
              actorId
          pure (UpdatedReplication x, actorMap)
    else do
      x <- getDestroyedReplicationValue
      pure (DestroyedReplication x, actorMap)

putReplicationValue :: ReplicationValue -> BinaryBit.BitPut ()
putReplicationValue value =
  case value of
    SpawnedReplication x -> do
      BinaryBit.putBool True
      BinaryBit.putBool True
      putSpawnedReplicationValue x
    UpdatedReplication x -> do
      BinaryBit.putBool True
      BinaryBit.putBool False
      putUpdatedReplicationValue x
    DestroyedReplication x -> do
      BinaryBit.putBool False
      putDestroyedReplicationValue x
