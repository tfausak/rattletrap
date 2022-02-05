module Rattletrap.Type.Attribute.PrivateMatchSettings where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data PrivateMatchSettings = PrivateMatchSettings
  { mutators :: Str.Str
  , joinableBy :: U32.U32
  , maxPlayers :: U32.U32
  , gameName :: Str.Str
  , password :: Str.Str
  , flag :: Bool
  }
  deriving (Eq, Show)

instance Argo.HasCodec PrivateMatchSettings where
  codec = Argo.identified .
    Argo.fromObjectCodec Argo.Allow
      $ PrivateMatchSettings
      <$> Argo.project
            mutators
            (Argo.required (Argo.fromString "mutators") Argo.codec)
      <*> Argo.project
            joinableBy
            (Argo.required (Argo.fromString "joinable_by") Argo.codec)
      <*> Argo.project
            maxPlayers
            (Argo.required (Argo.fromString "max_players") Argo.codec)
      <*> Argo.project
            gameName
            (Argo.required (Argo.fromString "game_name") Argo.codec)
      <*> Argo.project
            password
            (Argo.required (Argo.fromString "password") Argo.codec)
      <*> Argo.project flag (Argo.required (Argo.fromString "flag") Argo.codec)

bitPut :: PrivateMatchSettings -> BitPut.BitPut
bitPut privateMatchSettingsAttribute =
  Str.bitPut (mutators privateMatchSettingsAttribute)
    <> U32.bitPut (joinableBy privateMatchSettingsAttribute)
    <> U32.bitPut (maxPlayers privateMatchSettingsAttribute)
    <> Str.bitPut (gameName privateMatchSettingsAttribute)
    <> Str.bitPut (password privateMatchSettingsAttribute)
    <> BitPut.bool (flag privateMatchSettingsAttribute)

bitGet :: BitGet.BitGet PrivateMatchSettings
bitGet = BitGet.label "PrivateMatchSettings" $ do
  mutators <- BitGet.label "mutators" Str.bitGet
  joinableBy <- BitGet.label "joinableBy" U32.bitGet
  maxPlayers <- BitGet.label "maxPlayers" U32.bitGet
  gameName <- BitGet.label "gameName" Str.bitGet
  password <- BitGet.label "password" Str.bitGet
  flag <- BitGet.label "flag" BitGet.bool
  pure PrivateMatchSettings
    { mutators
    , joinableBy
    , maxPlayers
    , gameName
    , password
    , flag
    }
