module Rattletrap.Type.Replication where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Replication = Replication
  { actorId :: CompressedWord.CompressedWord
  , value :: ReplicationValue.ReplicationValue
  }
  deriving (Eq, Show)

instance Json.FromJSON Replication where
  parseJSON = Json.withObject "Replication" $ \object -> do
    actorId <- Json.required object "actor_id"
    value <- Json.required object "value"
    pure Replication { actorId, value }

instance Json.ToJSON Replication where
  toJSON x =
    Json.object [Json.pair "actor_id" $ actorId x, Json.pair "value" $ value x]

schema :: Schema.Schema
schema = Schema.named "replication" $ Schema.object
  [ (Json.pair "actor_id" $ Schema.ref CompressedWord.schema, True)
  , (Json.pair "value" $ Schema.ref ReplicationValue.schema, True)
  ]

putReplications :: List.List Replication -> BitPut.BitPut
putReplications xs =
  foldMap (\x -> BitPut.bool True <> bitPut x) (List.toList xs)
    <> BitPut.bool False

bitPut :: Replication -> BitPut.BitPut
bitPut replication = CompressedWord.bitPut (actorId replication)
  <> ReplicationValue.bitPut (value replication)

decodeReplicationsBits
  :: Maybe Str.Str
  -> Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> BitGet.BitGet
       ( Map.Map CompressedWord.CompressedWord U32.U32
       , List.List Replication
       )
decodeReplicationsBits matchType version limit classes actorMap =
  decodeReplicationsBitsWith matchType version limit classes actorMap []

decodeReplicationsBitsWith
  :: Maybe Str.Str
  -> Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> [Replication]
  -> BitGet.BitGet
       ( Map.Map CompressedWord.CompressedWord U32.U32
       , List.List Replication
       )
decodeReplicationsBitsWith matchType version limit classes actorMap replications
  = do
    hasReplication <- BitGet.bool
    if hasReplication
      then do
        (newActorMap, replication) <- bitGet
          matchType
          version
          limit
          classes
          actorMap
        decodeReplicationsBitsWith matchType version limit classes newActorMap
          $ replication
          : replications
      else pure (actorMap, List.fromList $ reverse replications)

bitGet
  :: Maybe Str.Str
  -> Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> BitGet.BitGet
       ( Map.Map CompressedWord.CompressedWord U32.U32
       , Replication
       )
bitGet matchType version limit classes actorMap = do
  actorId <- CompressedWord.bitGet limit
  (newActorMap, value) <- ReplicationValue.bitGet
    matchType
    version
    classes
    actorId
    actorMap
  pure (newActorMap, Replication { actorId, value })
