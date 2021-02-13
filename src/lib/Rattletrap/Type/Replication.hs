{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data Replication = Replication
  { actorId :: CompressedWord
  , value :: ReplicationValue.ReplicationValue
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Replication jsonOptions)

putReplications :: [Replication] -> BitPut ()
putReplications replications = case replications of
  [] -> BinaryBits.putBool False
  [replication] -> do
    bitPut replication
    BinaryBits.putBool False
  first : rest -> do
    bitPut first
    putReplications rest

bitPut :: Replication -> BitPut ()
bitPut replication = do
  BinaryBits.putBool True
  putCompressedWord (actorId replication)
  ReplicationValue.bitPut (value replication)

decodeReplicationsBits
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le.Word32le)
       BitGet
       [Replication]
decodeReplicationsBits version limit classes = do
  hasReplication <- Trans.lift getBool
  if hasReplication
    then
      (:)
      <$> bitGet version limit classes
      <*> decodeReplicationsBits version limit classes
    else pure []

bitGet
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le.Word32le)
       BitGet
       Replication
bitGet version limit classes = do
  actor <- Trans.lift (decodeCompressedWordBits limit)
  Replication actor <$> ReplicationValue.bitGet version classes actor
