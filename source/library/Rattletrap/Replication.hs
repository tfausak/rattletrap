module Rattletrap.Replication where

import Rattletrap.ClassAttributeMap
import Rattletrap.Map
import Rattletrap.Primitive
import Rattletrap.ReplicationValue

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Show)

getReplications
  :: (Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet ([Replication], ActorMap)
getReplications version maxChannels classAttributeMap actorMap = do
  maybeReplication <-
    getReplication version maxChannels classAttributeMap actorMap
  case maybeReplication of
    Nothing -> pure ([], actorMap)
    Just (replication, newActorMap) -> do
      (replications, newerActorMap) <-
        getReplications version maxChannels classAttributeMap newActorMap
      pure (replication : replications, newerActorMap)

putReplications :: [Replication] -> BinaryBit.BitPut ()
putReplications replications = do
  mapM_ putReplication replications
  BinaryBit.putBool False

getReplication
  :: (Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Maybe (Replication, ActorMap))
getReplication version maxChannels classAttributeMap actorMap = do
  hasReplication <- BinaryBit.getBool
  if not hasReplication
    then pure Nothing
    else do
      actorId <- getCompressedWord maxChannels
      (value, newActorMap) <-
        getReplicationValue version classAttributeMap actorMap actorId
      pure (Just (Replication actorId value, newActorMap))

putReplication :: Replication -> BinaryBit.BitPut ()
putReplication replication = do
  BinaryBit.putBool True
  putCompressedWord (replicationActorId replication)
  putReplicationValue (replicationValue replication)
