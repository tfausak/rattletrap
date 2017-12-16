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

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getReplicationValue
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> CompressedWord
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BinaryBit.BitGet
       ReplicationValue
getReplicationValue version classAttributeMap actorId = do
  isOpen <- Trans.lift BinaryBit.getBool
  if isOpen
    then do
      isNew <- Trans.lift BinaryBit.getBool
      if isNew
        then do
          x <- getSpawnedReplication version classAttributeMap actorId
          pure (ReplicationValueSpawned x)
        else do
          actorMap <- State.get
          x <- Trans.lift
            (getUpdatedReplication version classAttributeMap actorMap actorId)
          pure (ReplicationValueUpdated x)
    else do
      x <- Trans.lift decodeDestroyedReplicationBits
      pure (ReplicationValueDestroyed x)
