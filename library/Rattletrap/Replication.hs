module Rattletrap.Replication where

import Rattletrap.ClassAttributeMap
import Rattletrap.CompressedWord
import Rattletrap.ReplicationValue

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Ord, Show)

getReplications :: ClassAttributeMap -> BinaryBit.BitGet [Replication]
getReplications classAttributeMap = do
  maybeReplication <- getReplication classAttributeMap
  case maybeReplication of
    Nothing -> pure []
    Just replication -> do
      replications <- getReplications classAttributeMap
      pure (replication : replications)

putReplications :: [Replication] -> BinaryBit.BitPut ()
putReplications replications = do
  mapM_ putReplication replications
  BinaryBit.putBool False

getReplication :: ClassAttributeMap -> BinaryBit.BitGet (Maybe Replication)
getReplication classAttributeMap = do
  hasReplication <- BinaryBit.getBool
  if not hasReplication
    then pure Nothing
    else do
      actorId <- getCompressedWord maxActorId
      value <- getReplicationValue classAttributeMap actorId
      pure
        (Just
           Replication {replicationActorId = actorId, replicationValue = value})

putReplication :: Replication -> BinaryBit.BitPut ()
putReplication replication = do
  BinaryBit.putBool True
  putCompressedWord (replicationActorId replication)
  putReplicationValue (replicationValue replication)

maxActorId :: Word
maxActorId = 1023
