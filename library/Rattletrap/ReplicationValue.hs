module Rattletrap.ReplicationValue where

import Rattletrap.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ReplicationValue
  = SpawnedReplication Bool
                       Word32
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
          _ <- fail "get spawned replication value"
          pure (SpawnedReplication unknown objectId)
        else fail "get updated replication value"
    else pure DestroyedReplication

putReplicationValue :: ReplicationValue -> BinaryBit.BitPut ()
putReplicationValue value =
  case value of
    SpawnedReplication unknown objectId -> do
      BinaryBit.putBool True
      BinaryBit.putBool True
      BinaryBit.putBool unknown
      putWord32Bits objectId
      fail "put spawned replication value"
    UpdatedReplication -> do
      BinaryBit.putBool True
      BinaryBit.putBool False
      fail "put updated replication value"
    DestroyedReplication -> BinaryBit.putBool False
