{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.ReplicationValue
import Rattletrap.Encode.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBits

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Replication)

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
