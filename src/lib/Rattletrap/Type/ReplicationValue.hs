module Rattletrap.Type.ReplicationValue where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Replication.Destroyed as Destroyed
import qualified Rattletrap.Type.Replication.Spawned as Spawned
import qualified Rattletrap.Type.Replication.Updated as Updated
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data ReplicationValue
  = Spawned Spawned.Spawned
  -- ^ Creates a new actor.
  | Updated Updated.Updated
  -- ^ Updates an existing actor.
  | Destroyed Destroyed.Destroyed
  -- ^ Destroys an existing actor.
  deriving (Eq, Show)

instance Argo.HasCodec ReplicationValue where
  codec = Argo.identified $ Argo.oneOf
    [ Argo.mapMaybe
      (Just . Spawned)
      (\x -> case x of
        Spawned y -> Just y
        _ -> Nothing
      )
      . Argo.fromObjectCodec Argo.Forbid $ Argo.required id "spawned"
    , Argo.mapMaybe
      (Just . Updated)
      (\x -> case x of
        Updated y -> Just y
        _ -> Nothing
      )
      . Argo.fromObjectCodec Argo.Forbid $ Argo.required id "updated"
    , Argo.mapMaybe
      (Just . Destroyed)
      (\x -> case x of
        Destroyed y -> Just y
        _ -> Nothing
      )
      . Argo.fromObjectCodec Argo.Forbid $ Argo.required id "destroyed"
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
