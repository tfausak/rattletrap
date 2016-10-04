module Rattletrap.Replication where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.CompressedWord
import Rattletrap.ReplicationValue

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Ord, Show)

getReplications :: ClassAttributeMap
                -> ActorMap
                -> BinaryBit.BitGet ([Replication], ActorMap)
getReplications classAttributeMap actorMap = do
  maybeReplication <- getReplication classAttributeMap actorMap
  case maybeReplication of
    Nothing -> pure ([], actorMap)
    Just (replication, newActorMap) -> do
      (replications, newerActorMap) <-
        getReplications classAttributeMap newActorMap
      pure (replication : replications, newerActorMap)

putReplications :: [Replication] -> BinaryBit.BitPut ()
putReplications replications = do
  mapM_ putReplication replications
  BinaryBit.putBool False

getReplication
  :: ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Maybe (Replication, ActorMap))
getReplication classAttributeMap actorMap = do
  hasReplication <- BinaryBit.getBool
  if not hasReplication
    then pure Nothing
    else do
      actorId <- getCompressedWord maxActorId
      (value, newActorMap) <-
        getReplicationValue classAttributeMap actorMap actorId
      pure
        (Just
           ( Replication
             {replicationActorId = actorId, replicationValue = value}
           , newActorMap))

putReplication :: Replication -> BinaryBit.BitPut ()
putReplication replication = do
  BinaryBit.putBool True
  putCompressedWord (replicationActorId replication)
  putReplicationValue (replicationValue replication)

maxActorId :: Word
maxActorId = 1023
