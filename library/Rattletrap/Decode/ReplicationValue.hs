module Rattletrap.Decode.ReplicationValue
  ( getReplicationValue
  ) where

import Rattletrap.Decode.DestroyedReplication
import Rattletrap.Decode.SpawnedReplication
import Rattletrap.Decode.UpdatedReplication
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.ReplicationValue
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getReplicationValue
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> CompressedWord
  -> BinaryBit.BitGet
       (ReplicationValue, Map.Map CompressedWord Word32le)
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
          pure (ReplicationValueSpawned x, newActorMap)
        else do
          x <- getUpdatedReplication version classAttributeMap actorMap actorId
          pure (ReplicationValueUpdated x, actorMap)
    else do
      x <- getDestroyedReplication
      pure (ReplicationValueDestroyed x, actorMap)
