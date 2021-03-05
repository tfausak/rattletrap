module Rattletrap.Type.Replication where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

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
  :: Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       (List.List Replication)
decodeReplicationsBits version limit classes = List.untilM $ do
  p <- Trans.lift BitGet.bool
  if p then fmap Just $ bitGet version limit classes else pure Nothing

bitGet
  :: Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       Replication
bitGet version limit classes = do
  actor <- Trans.lift (CompressedWord.bitGet limit)
  fmap (Replication actor) $ ReplicationValue.bitGet version classes actor
