module Rattletrap.Type.ReplicationValue where

import qualified Data.Aeson as Aeson
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Replication.Destroyed as Destroyed
import qualified Rattletrap.Type.Replication.Spawned as Spawned
import qualified Rattletrap.Type.Replication.Updated as Updated
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

data ReplicationValue
  = Spawned Spawned.Spawned
  -- ^ Creates a new actor.
  | Updated Updated.Updated
  -- ^ Updates an existing actor.
  | Destroyed Destroyed.Destroyed
  -- ^ Destroys an existing actor.
  deriving (Eq, Show)

$(deriveJson ''ReplicationValue)

schema :: Schema.Schema
schema = Schema.named "replicationValue" $ Aeson.object
  [ Json.pair
      "oneOf"
      [ Schema.object [(Json.pair "spawned" $ Schema.ref Spawned.schema, True)]
      , Schema.object [(Json.pair "updated" $ Schema.json Schema.todo, True)]
      , Schema.object
        [(Json.pair "destroyed" $ Schema.ref Destroyed.schema, True)]
      ]
  ]

bitPut :: ReplicationValue -> BitPut.BitPut
bitPut value = case value of
  Spawned x -> BitPut.bool True <> BitPut.bool True <> Spawned.bitPut x
  Updated x -> BitPut.bool True <> BitPut.bool False <> Updated.bitPut x
  Destroyed x -> BitPut.bool False <> Destroyed.bitPut x

bitGet
  :: Version.Version
  -> ClassAttributeMap.ClassAttributeMap
  -> CompressedWord.CompressedWord
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       ReplicationValue
bitGet version classAttributeMap actorId = do
  actorMap <- State.get
  isOpen <- Trans.lift BitGet.bool
  if isOpen
    then do
      isNew <- Trans.lift BitGet.bool
      if isNew
        then Spawned <$> Spawned.bitGet version classAttributeMap actorId
        else Updated <$> Trans.lift
          (Updated.bitGet version classAttributeMap actorMap actorId)
    else Destroyed <$> Trans.lift Destroyed.bitGet
