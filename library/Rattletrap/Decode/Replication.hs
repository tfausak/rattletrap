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

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getReplications
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BinaryBit.BitGet
       [Replication]
getReplications version maxChannels classAttributeMap = do
  maybeReplication <- getReplication version maxChannels classAttributeMap
  case maybeReplication of
    Nothing -> pure []
    Just replication -> do
      replications <- getReplications version maxChannels classAttributeMap
      pure (replication : replications)

getReplication
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BinaryBit.BitGet
       (Maybe Replication)
getReplication version maxChannels classAttributeMap = do
  hasReplication <- Trans.lift BinaryBit.getBool
  if not hasReplication
    then pure Nothing
    else do
      actorId <- Trans.lift (decodeCompressedWordBits maxChannels)
      value <- getReplicationValue version classAttributeMap actorId
      pure (Just (Replication actorId value))
