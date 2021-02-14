{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ReplicationValue where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Replication.Destroyed as Destroyed
import qualified Rattletrap.Type.Replication.Spawned as Spawned
import qualified Rattletrap.Type.Replication.Updated as Updated
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ReplicationValue
  = Spawned Spawned.Spawned
  -- ^ Creates a new actor.
  | Updated Updated.Updated
  -- ^ Updates an existing actor.
  | Destroyed Destroyed.Destroyed
  -- ^ Destroys an existing actor.
  deriving (Eq, Show)

$(deriveJson ''ReplicationValue)

bitPut :: ReplicationValue -> BitPut ()
bitPut value = case value of
  Spawned x -> do
    BinaryBits.putBool True
    BinaryBits.putBool True
    Spawned.bitPut x
  Updated x -> do
    BinaryBits.putBool True
    BinaryBits.putBool False
    Updated.bitPut x
  Destroyed x -> do
    BinaryBits.putBool False
    Destroyed.bitPut x

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> CompressedWord.CompressedWord
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet
       ReplicationValue
bitGet version classAttributeMap actorId = do
  actorMap <- State.get
  isOpen <- Trans.lift getBool
  if isOpen
    then do
      isNew <- Trans.lift getBool
      if isNew
        then
          Spawned
            <$> Spawned.bitGet version classAttributeMap actorId
        else Updated <$> Trans.lift
          (Updated.bitGet
            version
            classAttributeMap
            actorMap
            actorId
          )
    else Destroyed
      <$> Trans.lift Destroyed.bitGet
