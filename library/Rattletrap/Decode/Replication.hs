module Rattletrap.Decode.Replication
  ( getReplications
  , getReplication
  ) where

import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.ReplicationValue
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Replication
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getReplications
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> BinaryBit.BitGet
       ([Replication], Map.Map CompressedWord Word32le)
getReplications version maxChannels classAttributeMap actorMap = do
  maybeReplication <- getReplication
    version
    maxChannels
    classAttributeMap
    actorMap
  case maybeReplication of
    Nothing -> pure ([], actorMap)
    Just (replication, newActorMap) -> do
      (replications, newerActorMap) <- getReplications
        version
        maxChannels
        classAttributeMap
        newActorMap
      pure (replication : replications, newerActorMap)

getReplication
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> BinaryBit.BitGet
       (Maybe (Replication, Map.Map CompressedWord Word32le))
getReplication version maxChannels classAttributeMap actorMap = do
  hasReplication <- BinaryBit.getBool
  if not hasReplication
    then pure Nothing
    else do
      actorId <- getCompressedWord maxChannels
      (value, newActorMap) <- getReplicationValue
        version
        classAttributeMap
        actorMap
        actorId
      pure (Just (Replication actorId value, newActorMap))
