module Rattletrap.Type.Attribute.UniqueId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data UniqueId = UniqueId
  { systemId :: U8.U8
  , remoteId :: RemoteId.RemoteId
  , localId :: U8.U8
  }
  deriving (Eq, Show)

instance Argo.HasCodec UniqueId where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ UniqueId
      <$> Argo.required systemId "system_id"
      <*> Argo.required remoteId "remote_id"
      <*> Argo.required localId "local_id"

bitPut :: UniqueId -> BitPut.BitPut
bitPut uniqueIdAttribute =
  U8.bitPut (systemId uniqueIdAttribute)
    <> RemoteId.bitPut (remoteId uniqueIdAttribute)
    <> U8.bitPut (localId uniqueIdAttribute)

bitGet :: Version.Version -> BitGet.BitGet UniqueId
bitGet version = BitGet.label "UniqueId" $ do
  systemId <- BitGet.label "systemId" U8.bitGet
  remoteId <- BitGet.label "remoteId" $ RemoteId.bitGet version systemId
  localId <- BitGet.label "localId" U8.bitGet
  pure UniqueId { systemId, remoteId, localId }
