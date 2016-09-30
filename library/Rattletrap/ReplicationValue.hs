module Rattletrap.ReplicationValue where

import Rattletrap.Attribute
import Rattletrap.Initialization
import Rattletrap.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ReplicationValue
  = SpawnedReplication Bool
                       Word32
                       Initialization
  | UpdatedReplication [Attribute]
  | DestroyedReplication
  deriving (Eq, Ord, Show)

getReplicationValue :: BinaryBit.BitGet ReplicationValue
getReplicationValue = do
  isOpen <- BinaryBit.getBool
  if isOpen
    then do
      isNew <- BinaryBit.getBool
      if isNew
        then do
          unknown <- BinaryBit.getBool
          objectId <- getWord32Bits
          let hasLocation = error "spawned replication class has location"
          let hasRotation = error "spawned replication class has rotation"
          initialization <- getInitialization hasLocation hasRotation
          pure (SpawnedReplication unknown objectId initialization)
        else do
          let limit = error "updated replication attribute id limit"
          let getName _id = error "updated replication get attribute name"
          attributes <- getAttributes limit getName
          pure (UpdatedReplication attributes)
    else pure DestroyedReplication

putReplicationValue :: ReplicationValue -> BinaryBit.BitPut ()
putReplicationValue value =
  case value of
    SpawnedReplication unknown objectId initialization -> do
      BinaryBit.putBool True
      BinaryBit.putBool True
      BinaryBit.putBool unknown
      putWord32Bits objectId
      putInitialization initialization
    UpdatedReplication attributes -> do
      BinaryBit.putBool True
      BinaryBit.putBool False
      putAttributes attributes
    DestroyedReplication -> BinaryBit.putBool False
