module Rattletrap.Type.Attribute.UniqueId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data UniqueId = UniqueId
  { systemId :: U8.U8
  , remoteId :: RemoteId.RemoteId
  , localId :: U8.U8
  }
  deriving (Eq, Show)

instance Json.FromJSON UniqueId where
  parseJSON = Json.withObject "UniqueId" $ \object -> do
    systemId <- Json.required object "system_id"
    remoteId <- Json.required object "remote_id"
    localId <- Json.required object "local_id"
    pure UniqueId { systemId, remoteId, localId }

instance Json.ToJSON UniqueId where
  toJSON x = Json.object
    [ Json.pair "system_id" $ systemId x
    , Json.pair "remote_id" $ remoteId x
    , Json.pair "local_id" $ localId x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-unique-id" $ Schema.object
  [ (Json.pair "system_id" $ Schema.ref U8.schema, True)
  , (Json.pair "remote_id" $ Schema.ref RemoteId.schema, True)
  , (Json.pair "local_id" $ Schema.ref U8.schema, True)
  ]

bitPut :: UniqueId -> BitPut.BitPut
bitPut uniqueIdAttribute =
  U8.bitPut (systemId uniqueIdAttribute)
    <> RemoteId.bitPut (remoteId uniqueIdAttribute)
    <> U8.bitPut (localId uniqueIdAttribute)

bitGet :: Version.Version -> BitGet.BitGet UniqueId
bitGet version = do
  systemId <- U8.bitGet
  remoteId <- RemoteId.bitGet version systemId
  localId <- U8.bitGet
  pure UniqueId { systemId, remoteId, localId }
