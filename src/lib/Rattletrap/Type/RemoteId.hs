module Rattletrap.Type.RemoteId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.UnknownSystemId as UnknownSystemId
import qualified Rattletrap.Type.RemoteId.Epic as Epic
import qualified Rattletrap.Type.RemoteId.PlayStation as PlayStation
import qualified Rattletrap.Type.RemoteId.PsyNet as PsyNet
import qualified Rattletrap.Type.RemoteId.Splitscreen as Splitscreen
import qualified Rattletrap.Type.RemoteId.Steam as Steam
import qualified Rattletrap.Type.RemoteId.Switch as Switch
import qualified Rattletrap.Type.RemoteId.Xbox as Xbox
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

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

instance Argo.HasCodec RemoteId where
  codec =
    Argo.mapMaybe (Just . PlayStation) (\ x -> case x of { PlayStation y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "play_station") Argo.codec))
    Argo.<|> Argo.mapMaybe (Just . PsyNet) (\ x -> case x of { PsyNet y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "psy_net") Argo.codec))
    Argo.<|> Argo.mapMaybe (Just . Splitscreen) (\ x -> case x of { Splitscreen y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "splitscreen") Argo.codec))
    Argo.<|> Argo.mapMaybe (Just . Steam) (\ x -> case x of { Steam y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "steam") Argo.codec))
    Argo.<|> Argo.mapMaybe (Just . Switch) (\ x -> case x of { Switch y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "switch") Argo.codec))
    Argo.<|> Argo.mapMaybe (Just . Xbox) (\ x -> case x of { Xbox y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "xbox") Argo.codec))
    Argo.<|> Argo.mapMaybe (Just . Epic) (\ x -> case x of { Epic y -> Just y; _ -> Nothing }) (Argo.fromObjectCodec Argo.Allow (Argo.required (Argo.fromString "epic") Argo.codec))

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
