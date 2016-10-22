module Rattletrap.ReplicationValue.Destroyed where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data DestroyedReplicationValue =
  DestroyedReplicationValue
  deriving (Eq, Ord, Show)

getDestroyedReplicationValue :: BinaryBit.BitGet DestroyedReplicationValue
getDestroyedReplicationValue = pure DestroyedReplicationValue

putDestroyedReplicationValue :: DestroyedReplicationValue -> BinaryBit.BitPut ()
putDestroyedReplicationValue DestroyedReplicationValue = pure ()
