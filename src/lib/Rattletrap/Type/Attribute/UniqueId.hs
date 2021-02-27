module Rattletrap.Type.Attribute.UniqueId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
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

$(deriveJson ''UniqueId)

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
  systemId_ <- U8.bitGet
  UniqueId systemId_ <$> RemoteId.bitGet version systemId_ <*> U8.bitGet
