module Rattletrap.Replication where

import Rattletrap.ClassPropertyMap
import Rattletrap.CompressedWord
import Rattletrap.ReplicationValue

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Ord, Show)

getReplications :: ClassPropertyMap -> BinaryBit.BitGet [Replication]
getReplications classPropertyMap = do
  maybeReplication <- getReplication classPropertyMap
  case maybeReplication of
    Nothing -> pure []
    Just replication -> do
      replications <- getReplications classPropertyMap
      pure (replication : replications)

putReplications :: [Replication] -> BinaryBit.BitPut ()
putReplications replications = do
  mapM_ putReplication replications
  BinaryBit.putBool False

getReplication :: ClassPropertyMap -> BinaryBit.BitGet (Maybe Replication)
getReplication classPropertyMap = do
  hasReplication <- BinaryBit.getBool
  if not hasReplication
    then pure Nothing
    else do
      actorId <- getCompressedWord maxActorId
      value <- getReplicationValue classPropertyMap actorId
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
