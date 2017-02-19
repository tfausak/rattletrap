module Rattletrap.Replication where

import Rattletrap.Map
import Rattletrap.Primitive
import Rattletrap.ReplicationValue

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Vector as Vector

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Show)

getReplications
  :: (Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Vector.Vector Replication, ActorMap)
getReplications version maxChannels classAttributeMap actorMap =
  let go replications actors = do
        maybeReplication <-
          getReplication version maxChannels classAttributeMap actors
        case maybeReplication of
          Nothing -> pure (Vector.fromList (reverse replications), actors)
          Just (replication, newActors) ->
            go (replication : replications) newActors
  in go [] actorMap

putReplications :: Vector.Vector Replication -> BinaryBit.BitPut ()
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
