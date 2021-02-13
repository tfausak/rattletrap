{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ReplicationValue where

import Rattletrap.Type.Common
import Rattletrap.Type.Replication.Destroyed
import Rattletrap.Type.Replication.Spawned
import Rattletrap.Type.Replication.Updated
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ReplicationValue
  = ReplicationValueSpawned SpawnedReplication
  -- ^ Creates a new actor.
  | ReplicationValueUpdated UpdatedReplication
  -- ^ Updates an existing actor.
  | ReplicationValueDestroyed DestroyedReplication
  -- ^ Destroys an existing actor.
  deriving (Eq, Show)

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

decodeReplicationValueBits
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> CompressedWord
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BitGet
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
          (decodeUpdatedReplicationBits
            version
            classAttributeMap
            actorMap
            actorId
          )
    else ReplicationValueDestroyed
      <$> Trans.lift decodeDestroyedReplicationBits
