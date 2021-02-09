module Rattletrap.Decode.Replication
  ( decodeReplicationsBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.ReplicationValue
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Replication
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

decodeReplicationsBits
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       DecodeBits
       [Replication]
decodeReplicationsBits version limit classes = do
  hasReplication <- Trans.lift getBool
  if hasReplication
    then
      (:)
      <$> decodeReplicationBits version limit classes
      <*> decodeReplicationsBits version limit classes
    else pure []

decodeReplicationBits
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       DecodeBits
       Replication
decodeReplicationBits version limit classes = do
  actor <- Trans.lift (decodeCompressedWordBits limit)
  Replication actor <$> decodeReplicationValueBits version classes actor
