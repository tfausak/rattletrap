module Rattletrap.Type.Attribute.PartyLeader where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data PartyLeader = PartyLeader
  { systemId :: U8.U8
  , remoteId :: Maybe RemoteId.RemoteId
  , localId :: Maybe U8.U8
  }
  deriving (Eq, Show)

instance Argo.HasCodec PartyLeader where
  codec =
    Argo.identified
      . Argo.map
          (\(x, y) -> PartyLeader x (fmap fst y) (fmap snd y))
          (\x -> (systemId x, (,) <$> remoteId x <*> localId x))
      . Argo.fromObjectCodec Argo.Allow
      $ (,)
      <$> Argo.required fst "system_id"
      <*> Argo.optional snd "id"

bitPut :: PartyLeader -> BitPut.BitPut
bitPut x =
  U8.bitPut (systemId x) <> foldMap RemoteId.bitPut (remoteId x) <> foldMap
    U8.bitPut
    (localId x)

bitGet :: Version.Version -> BitGet.BitGet PartyLeader
bitGet version = BitGet.label "PartyLeader" $ do
  systemId <- BitGet.label "systemId" U8.bitGet
  (remoteId, localId) <- if systemId == U8.fromWord8 0
    then pure (Nothing, Nothing)
    else do
      remoteId <- BitGet.label "remoteId" $ RemoteId.bitGet version systemId
      localId <- BitGet.label "localId" U8.bitGet
      pure (Just remoteId, Just localId)
  pure PartyLeader { systemId, remoteId, localId }
