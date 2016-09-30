module Rattletrap.ReplicationValue where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ReplicationValue
  = SpawnedReplication
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
        then fail "get spawned replication value"
        else fail "get updated replication value"
    else pure DestroyedReplication

putReplicationValue :: ReplicationValue -> BinaryBit.BitPut ()
putReplicationValue value =
  case value of
    SpawnedReplication -> do
      BinaryBit.putBool True
      BinaryBit.putBool True
      fail "put spawned replication value"
    UpdatedReplication -> do
      BinaryBit.putBool True
      BinaryBit.putBool False
      fail "put updated replication value"
    DestroyedReplication -> BinaryBit.putBool False
