{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

data Replication = Replication
  { actorId :: CompressedWord.CompressedWord
  , value :: ReplicationValue.ReplicationValue
  }
  deriving (Eq, Show)

$(deriveJson ''Replication)

putReplications :: List.List Replication -> BitPut.BitPut
putReplications xs =
  foldMap
    (\ x -> BitPut.bool True <> bitPut x)
    (List.toList xs)
  <> BitPut.bool False

bitPut :: Replication -> BitPut.BitPut
bitPut replication =
  CompressedWord.bitPut (actorId replication)
  <> ReplicationValue.bitPut (value replication)

decodeReplicationsBits
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       (List.List Replication)
decodeReplicationsBits version limit classes = List.untilM $ do
  p <- Trans.lift BitGet.bool
  if p
    then Just <$> bitGet version limit classes
    else pure Nothing

bitGet
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       Replication
bitGet version limit classes = do
  actor <- Trans.lift (CompressedWord.bitGet limit)
  Replication actor <$> ReplicationValue.bitGet version classes actor
