module Rattletrap.Decode.ReplicationValue
  ( decodeReplicationValueBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.DestroyedReplication
import Rattletrap.Decode.SpawnedReplication
import Rattletrap.Decode.UpdatedReplication
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.ReplicationValue
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

decodeReplicationValueBits
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> CompressedWord
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       DecodeBits
       ReplicationValue
decodeReplicationValueBits version classAttributeMap actorId = do
  actorMap <- State.get
  isOpen <- Trans.lift getBool
  if isOpen
    then do
      isNew <- Trans.lift getBool
      if isNew
        then
          ReplicationValueSpawned
            <$> decodeSpawnedReplicationBits version classAttributeMap actorId
        else ReplicationValueUpdated <$> Trans.lift
          ( decodeUpdatedReplicationBits
            version
            classAttributeMap
            actorMap
            actorId
          )
    else ReplicationValueDestroyed
      <$> Trans.lift decodeDestroyedReplicationBits
