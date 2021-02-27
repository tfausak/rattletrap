module Rattletrap.Type.Attribute.PartyLeader where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import Rattletrap.Utility.Monad
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data PartyLeader = PartyLeader
  { systemId :: U8.U8
  , id :: Maybe (RemoteId.RemoteId, U8.U8)
  }
  deriving (Eq, Show)

$(deriveJson ''PartyLeader)

schema :: Schema.Schema
schema = Schema.named "attribute-party-leader" $ Schema.object
  [ (Json.pair "system_id" $ Schema.ref U8.schema, True)
  , ( Json.pair "id" $ Schema.oneOf
    [ Schema.tuple
        [ Schema.ref RemoteId.schema
        , Schema.ref U8.schema
        ]
    , Schema.ref Schema.null
    ]
    , False
    )
  ]

bitPut :: PartyLeader -> BitPut.BitPut
bitPut x = U8.bitPut (systemId x) <> foldMap
  (\(y, z) -> RemoteId.bitPut y <> U8.bitPut z)
  (Rattletrap.Type.Attribute.PartyLeader.id x)

bitGet :: Version.Version -> BitGet.BitGet PartyLeader
bitGet version = do
  systemId_ <- U8.bitGet
  PartyLeader systemId_ <$> whenMaybe
    (systemId_ /= U8.fromWord8 0)
    ((,) <$> RemoteId.bitGet version systemId_ <*> U8.bitGet)
