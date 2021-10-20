module Rattletrap.Type.ReplicationValue where

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Replication.Destroyed as Destroyed
import qualified Rattletrap.Type.Replication.Spawned as Spawned
import qualified Rattletrap.Type.Replication.Updated as Updated
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data ReplicationValue
  = Spawned Spawned.Spawned
  -- ^ Creates a new actor.
  | Updated Updated.Updated
  -- ^ Updates an existing actor.
  | Destroyed Destroyed.Destroyed
  -- ^ Destroys an existing actor.
  deriving (Eq, Show)

instance Json.FromValue ReplicationValue where
  fromValue = Json.withObject "ReplicationValue" $ \object -> Foldable.asum
    [ fmap Spawned $ Json.required object "spawned"
    , fmap Updated $ Json.required object "updated"
    , fmap Destroyed $ Json.required object "destroyed"
    ]

instance Json.ToValue ReplicationValue where
  toValue x = case x of
    Spawned y -> Json.object [Json.pair "spawned" y]
    Updated y -> Json.object [Json.pair "updated" y]
    Destroyed y -> Json.object [Json.pair "destroyed" y]

schema :: Schema.Schema
schema = Schema.named "replicationValue" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k $ Schema.ref v, True)])
  [ ("spawned", Spawned.schema)
  , ("updated", Updated.schema)
  , ("destroyed", Destroyed.schema)
  ]

bitPut :: ReplicationValue -> BitPut.BitPut
bitPut value = case value of
  Spawned x -> BitPut.bool True <> BitPut.bool True <> Spawned.bitPut x
  Updated x -> BitPut.bool True <> BitPut.bool False <> Updated.bitPut x
  Destroyed x -> BitPut.bool False <> Destroyed.bitPut x

bitGet
  :: Maybe Str.Str
  -> Version.Version
  -> ClassAttributeMap.ClassAttributeMap
  -> CompressedWord.CompressedWord
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> BitGet.BitGet
       ( Map.Map CompressedWord.CompressedWord U32.U32
       , ReplicationValue
       )
bitGet matchType version classAttributeMap actorId actorMap =
  BitGet.label "ReplicationValue" $ do
    isOpen <- BitGet.bool
    if isOpen
      then do
        isNew <- BitGet.bool
        if isNew
          then do
            (newActorMap, spawned) <- Spawned.bitGet
              matchType
              version
              classAttributeMap
              actorId
              actorMap
            pure (newActorMap, Spawned spawned)
          else do
            updated <- Updated.bitGet
              version
              classAttributeMap
              actorMap
              actorId
            pure (actorMap, Updated updated)
      else do
        destroyed <- Destroyed.bitGet
        pure (actorMap, Destroyed destroyed)
