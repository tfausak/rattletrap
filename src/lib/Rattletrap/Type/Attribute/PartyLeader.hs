module Rattletrap.Type.Attribute.PartyLeader where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data PartyLeader = PartyLeader
  { systemId :: U8.U8
  , remoteId :: Maybe RemoteId.RemoteId
  , localId :: Maybe U8.U8
  }
  deriving (Eq, Show)

instance Json.FromJSON PartyLeader where
  parseJSON = Json.withObject "PartyLeader" $ \object -> do
    systemId <- Json.required object "system_id"
    maybeId <- Json.optional object "id"
    pure PartyLeader
      { systemId
      , remoteId = fmap fst maybeId
      , localId = fmap snd maybeId
      }

instance Json.ToJSON PartyLeader where
  toJSON x = Json.object
    [ Json.pair "system_id" $ systemId x
    , Json.pair "id" $ case (remoteId x, localId x) of
      (Just r, Just l) -> Just (r, l)
      _ -> Nothing
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-party-leader" $ Schema.object
  [ (Json.pair "system_id" $ Schema.ref U8.schema, True)
  , ( Json.pair "id" $ Schema.oneOf
      [ Schema.tuple [Schema.ref RemoteId.schema, Schema.ref U8.schema]
      , Schema.ref Schema.null
      ]
    , False
    )
  ]

bitPut :: PartyLeader -> BitPut.BitPut
bitPut x =
  U8.bitPut (systemId x) <> foldMap RemoteId.bitPut (remoteId x) <> foldMap
    U8.bitPut
    (localId x)

bitGet :: Version.Version -> BitGet.BitGet PartyLeader
bitGet version = do
  systemId <- U8.bitGet
  (remoteId, localId) <- if systemId == U8.fromWord8 0
    then pure (Nothing, Nothing)
    else do
      remoteId <- RemoteId.bitGet version systemId
      localId <- U8.bitGet
      pure (Just remoteId, Just localId)
  pure PartyLeader { systemId, remoteId, localId }
