module Rattletrap.ReplicationValue where

import Rattletrap.Initialization
import Rattletrap.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ReplicationValue
  = SpawnedReplication Bool
                       Word32
                       Initialization
  | UpdatedReplication
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
          let hasLocation = error "has location"
          let hasRotation = error "has rotation"
          initialization <- getInitialization hasLocation hasRotation
          pure (SpawnedReplication unknown objectId initialization)
        else fail "get updated replication value"
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
    UpdatedReplication -> do
      BinaryBit.putBool True
      BinaryBit.putBool False
      fail "put updated replication value"
    DestroyedReplication -> BinaryBit.putBool False
