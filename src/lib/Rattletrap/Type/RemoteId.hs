module Rattletrap.Type.RemoteId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.UnknownSystemId as UnknownSystemId
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.RemoteId.Epic as Epic
import qualified Rattletrap.Type.RemoteId.PlayStation as PlayStation
import qualified Rattletrap.Type.RemoteId.PsyNet as PsyNet
import qualified Rattletrap.Type.RemoteId.Splitscreen as Splitscreen
import qualified Rattletrap.Type.RemoteId.Steam as Steam
import qualified Rattletrap.Type.RemoteId.Switch as Switch
import qualified Rattletrap.Type.RemoteId.Xbox as Xbox
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

import qualified Data.Foldable as Foldable

data RemoteId
  = PlayStation PlayStation.PlayStation
  | PsyNet PsyNet.PsyNet
  | Splitscreen Splitscreen.Splitscreen
  -- ^ Really only 24 bits.
  | Steam Steam.Steam
  | Switch Switch.Switch
  | Xbox Xbox.Xbox
  | Epic Epic.Epic
  deriving (Eq, Show)

instance Json.FromJSON RemoteId where
  parseJSON = Json.withObject "RemoteId" $ \object -> Foldable.asum
    [ fmap PlayStation $ Json.required object "play_station"
    , fmap PsyNet $ Json.required object "psy_net"
    , fmap Splitscreen $ Json.required object "splitscreen"
    , fmap Steam $ Json.required object "steam"
    , fmap Switch $ Json.required object "switch"
    , fmap Xbox $ Json.required object "xbox"
    , fmap Epic $ Json.required object "epic"
    ]

instance Json.ToJSON RemoteId where
  toJSON x = case x of
    PlayStation y -> Json.object [Json.pair "play_station" y]
    PsyNet y -> Json.object [Json.pair "psy_net" y]
    Splitscreen y -> Json.object [Json.pair "splitscreen" y]
    Steam y -> Json.object [Json.pair "steam" y]
    Switch y -> Json.object [Json.pair "switch" y]
    Xbox y -> Json.object [Json.pair "xbox" y]
    Epic y -> Json.object [Json.pair "epic" y]

schema :: Schema.Schema
schema = Schema.named "remote-id" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k v, True)])
  [ ("play_station", Schema.ref PlayStation.schema)
  , ("psy_net", Schema.ref PsyNet.schema)
  , ("splitscreen", Schema.ref Splitscreen.schema)
  , ("steam", Schema.ref Steam.schema)
  , ("switch", Schema.ref Switch.schema)
  , ("xbox", Schema.ref Xbox.schema)
  , ("epic", Schema.ref Epic.schema)
  ]

bitPut :: RemoteId -> BitPut.BitPut
bitPut remoteId = case remoteId of
  PlayStation x -> PlayStation.bitPut x
  PsyNet x -> PsyNet.bitPut x
  Splitscreen x -> Splitscreen.bitPut x
  Steam x -> Steam.bitPut x
  Switch x -> Switch.bitPut x
  Xbox x -> Xbox.bitPut x
  Epic x -> Epic.bitPut x

bitGet :: Version.Version -> U8.U8 -> BitGet.BitGet RemoteId
bitGet version systemId = case U8.toWord8 systemId of
  0 -> fmap Splitscreen Splitscreen.bitGet
  1 -> fmap Steam Steam.bitGet
  2 -> fmap PlayStation $ PlayStation.bitGet version
  4 -> fmap Xbox Xbox.bitGet
  6 -> fmap Switch Switch.bitGet
  7 -> fmap PsyNet $ PsyNet.bitGet version
  11 -> fmap Epic Epic.bitGet
  x -> BitGet.throw $ UnknownSystemId.UnknownSystemId x
