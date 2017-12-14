module Rattletrap.Decode.ReplicationValue
  ( getReplicationValue
  ) where

import Rattletrap.Type.ActorMap
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Decode.DestroyedReplication
import Rattletrap.Decode.SpawnedReplication
import Rattletrap.Type.ReplicationValue
import Rattletrap.Decode.UpdatedReplication

import qualified Data.Binary.Bits.Get as BinaryBit

getReplicationValue
  :: (Int, Int, Int)
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
          (x, newActorMap) <- getSpawnedReplication
            version
            classAttributeMap
            actorMap
            actorId
          pure (SpawnedReplicationValue x, newActorMap)
        else do
          x <- getUpdatedReplication version classAttributeMap actorMap actorId
          pure (UpdatedReplicationValue x, actorMap)
    else do
      x <- getDestroyedReplication
      pure (DestroyedReplicationValue x, actorMap)
