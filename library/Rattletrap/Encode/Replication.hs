module Rattletrap.Encode.Replication
  ( putReplications
  , putReplication
  ) where

import Rattletrap.Type.Replication
import Rattletrap.Encode.CompressedWord
import Rattletrap.Encode.ReplicationValue

import qualified Data.Binary.Bits.Put as BinaryBit

putReplications :: [Replication] -> BinaryBit.BitPut ()
putReplications replications = do
  mapM_ putReplication replications
  BinaryBit.putBool False

putReplication :: Replication -> BinaryBit.BitPut ()
putReplication replication = do
  BinaryBit.putBool True
  putCompressedWord (replicationActorId replication)
  putReplicationValue (replicationValue replication)
