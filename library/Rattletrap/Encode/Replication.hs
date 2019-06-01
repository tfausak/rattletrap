module Rattletrap.Encode.Replication
  ( putReplications
  )
where

import Rattletrap.Encode.CompressedWord
import Rattletrap.Encode.ReplicationValue
import Rattletrap.Type.Replication

import qualified Data.Binary.Bits.Put as BinaryBits

putReplications :: [Replication] -> BinaryBits.BitPut ()
putReplications replications = case replications of
  [] -> BinaryBits.putBool False
  [replication] -> do
    putReplication replication
    BinaryBits.putBool False
  first : rest -> do
    putReplication first
    putReplications rest

putReplication :: Replication -> BinaryBits.BitPut ()
putReplication replication = do
  BinaryBits.putBool True
  putCompressedWord (replicationActorId replication)
  putReplicationValue (replicationValue replication)
