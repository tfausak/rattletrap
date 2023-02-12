module Rattletrap.Type.Attribute.PrivateMatchSettings where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data PrivateMatchSettings = PrivateMatchSettings
  { mutators :: Str.Str,
    joinableBy :: U32.U32,
    maxPlayers :: U32.U32,
    gameName :: Str.Str,
    password :: Str.Str,
    flag :: Bool
  }
  deriving (Eq, Show)

instance Json.FromJSON PrivateMatchSettings where
  parseJSON = Json.withObject "PrivateMatchSettings" $ \object -> do
    mutators <- Json.required object "mutators"
    joinableBy <- Json.required object "joinable_by"
    maxPlayers <- Json.required object "max_players"
    gameName <- Json.required object "game_name"
    password <- Json.required object "password"
    flag <- Json.required object "flag"
    pure
      PrivateMatchSettings
        { mutators,
          joinableBy,
          maxPlayers,
          gameName,
          password,
          flag
        }

instance Json.ToJSON PrivateMatchSettings where
  toJSON x =
    Json.object
      [ Json.pair "mutators" $ mutators x,
        Json.pair "joinable_by" $ joinableBy x,
        Json.pair "max_players" $ maxPlayers x,
        Json.pair "game_name" $ gameName x,
        Json.pair "password" $ password x,
        Json.pair "flag" $ flag x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute-private-match-settings" $
    Schema.object
      [ (Json.pair "mutators" $ Schema.ref Str.schema, True),
        (Json.pair "joinable_by" $ Schema.ref U32.schema, True),
        (Json.pair "max_players" $ Schema.ref U32.schema, True),
        (Json.pair "game_name" $ Schema.ref Str.schema, True),
        (Json.pair "password" $ Schema.ref Str.schema, True),
        (Json.pair "flag" $ Schema.ref Schema.boolean, True)
      ]

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
  pure
    PrivateMatchSettings
      { mutators,
        joinableBy,
        maxPlayers,
        gameName,
        password,
        flag
      }
