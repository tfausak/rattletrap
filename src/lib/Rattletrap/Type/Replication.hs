module Rattletrap.Type.Replication where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Replication = Replication
  { actorId :: CompressedWord.CompressedWord
  , value :: ReplicationValue.ReplicationValue
  }
  deriving (Eq, Show)

instance Argo.HasCodec Replication where
  codec =
    Argo.fromObjectCodec Argo.Allow
      $ Replication
      <$> Argo.project
            actorId
            (Argo.required (Argo.fromString "actor_id") Argo.codec)
      <*> Argo.project
            value
            (Argo.required (Argo.fromString "value") Argo.codec)

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
  decodeReplicationsBitsWith matchType version limit classes actorMap 0 []

decodeReplicationsBitsWith
  :: Maybe Str.Str
  -> Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> Int
  -> [Replication]
  -> BitGet.BitGet
       ( Map.Map CompressedWord.CompressedWord U32.U32
       , List.List Replication
       )
decodeReplicationsBitsWith matchType version limit classes actorMap index replications
  = do
    hasReplication <- BitGet.bool
    if hasReplication
      then do
        (newActorMap, replication) <-
          BitGet.label ("element (" <> show index <> ")")
            $ bitGet matchType version limit classes actorMap
        decodeReplicationsBitsWith
            matchType
            version
            limit
            classes
            newActorMap
            (index + 1)
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
bitGet matchType version limit classes actorMap =
  BitGet.label "Replication" $ do
    actorId <- BitGet.label "actorId" $ CompressedWord.bitGet limit
    (newActorMap, value) <- BitGet.label "value"
      $ ReplicationValue.bitGet matchType version classes actorId actorMap
    pure (newActorMap, Replication { actorId, value })
