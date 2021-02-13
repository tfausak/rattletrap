{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.ReplicationValue
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.Word32le
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  }
  deriving (Eq, Show)

$(deriveJson ''Replication)

putReplications :: [Replication] -> BitPut ()
putReplications replications = case replications of
  [] -> BinaryBits.putBool False
  [replication] -> do
    putReplication replication
    BinaryBits.putBool False
  first : rest -> do
    putReplication first
    putReplications rest

putReplication :: Replication -> BitPut ()
putReplication replication = do
  BinaryBits.putBool True
  putCompressedWord (replicationActorId replication)
  putReplicationValue (replicationValue replication)

decodeReplicationsBits
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BitGet
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
       BitGet
       Replication
decodeReplicationBits version limit classes = do
  actor <- Trans.lift (decodeCompressedWordBits limit)
  Replication actor <$> decodeReplicationValueBits version classes actor
