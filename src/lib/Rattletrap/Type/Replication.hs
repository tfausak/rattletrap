module Rattletrap.Type.Replication where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as RList
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Replication = Replication
  { actorId :: CompressedWord.CompressedWord,
    value :: ReplicationValue.ReplicationValue
  }
  deriving (Eq, Show)

instance Json.FromJSON Replication where
  parseJSON = Json.withObject "Replication" $ \object -> do
    actorId <- Json.required object "actor_id"
    value <- Json.required object "value"
    pure Replication {actorId, value}

instance Json.ToJSON Replication where
  toJSON x =
    Json.object [Json.pair "actor_id" $ actorId x, Json.pair "value" $ value x]

schema :: Schema.Schema
schema =
  Schema.named "replication" $
    Schema.object
      [ (Json.pair "actor_id" $ Schema.ref CompressedWord.schema, True),
        (Json.pair "value" $ Schema.ref ReplicationValue.schema, True)
      ]

putReplications :: RList.List Replication -> BitPut.BitPut
putReplications xs =
  foldMap (\x -> BitPut.bool True <> bitPut x) (RList.toList xs)
    <> BitPut.bool False

bitPut :: Replication -> BitPut.BitPut
bitPut replication =
  CompressedWord.bitPut (actorId replication)
    <> ReplicationValue.bitPut (value replication)

decodeReplicationsBits ::
  Maybe Str.Str ->
  Version.Version ->
  Maybe Str.Str ->
  Word ->
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  BitGet.BitGet
    ( Map.Map CompressedWord.CompressedWord U32.U32,
      RList.List Replication
    )
decodeReplicationsBits matchType version buildVersion limit classes actorMap =
  decodeReplicationsBitsWith
    matchType
    version
    buildVersion
    limit
    classes
    actorMap
    0
    []

decodeReplicationsBitsWith ::
  Maybe Str.Str ->
  Version.Version ->
  Maybe Str.Str ->
  Word ->
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  Int ->
  [Replication] ->
  BitGet.BitGet
    ( Map.Map CompressedWord.CompressedWord U32.U32,
      RList.List Replication
    )
decodeReplicationsBitsWith matchType version buildVersion limit classes actorMap index replications =
  do
    hasReplication <- BitGet.bool
    if hasReplication
      then do
        (newActorMap, replication) <-
          BitGet.label ("element (" <> show index <> ")") $
            bitGet matchType version buildVersion limit classes actorMap
        decodeReplicationsBitsWith
          matchType
          version
          buildVersion
          limit
          classes
          newActorMap
          (index + 1)
          $ replication
            : replications
      else pure (actorMap, RList.fromList $ reverse replications)

bitGet ::
  Maybe Str.Str ->
  Version.Version ->
  Maybe Str.Str ->
  Word ->
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  BitGet.BitGet
    ( Map.Map CompressedWord.CompressedWord U32.U32,
      Replication
    )
bitGet matchType version buildVersion limit classes actorMap =
  BitGet.label "Replication" $ do
    actorId <- BitGet.label "actorId" $ CompressedWord.bitGet limit
    (newActorMap, value) <-
      BitGet.label "value" $
        ReplicationValue.bitGet
          matchType
          version
          buildVersion
          classes
          actorId
          actorMap
    pure (newActorMap, Replication {actorId, value})
