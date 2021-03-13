module Rattletrap.Type.Attribute.Loadout where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data Loadout = Loadout
  { version :: U8.U8
  , body :: U32.U32
  , decal :: U32.U32
  , wheels :: U32.U32
  , rocketTrail :: U32.U32
  -- ^ Now known as "rocket boost".
  , antenna :: U32.U32
  , topper :: U32.U32
  , unknown1 :: U32.U32
  , unknown2 :: Maybe U32.U32
  , engineAudio :: Maybe U32.U32
  , trail :: Maybe U32.U32
  , goalExplosion :: Maybe U32.U32
  , banner :: Maybe U32.U32
  , unknown3 :: Maybe U32.U32
  , unknown4 :: Maybe U32.U32
  , unknown5 :: Maybe U32.U32
  , unknown6 :: Maybe U32.U32
  }
  deriving (Eq, Show)

instance Json.FromJSON Loadout where
  parseJSON = Json.withObject "Loadout" $ \object -> do
    version <- Json.required object "version"
    body <- Json.required object "body"
    decal <- Json.required object "decal"
    wheels <- Json.required object "wheels"
    rocketTrail <- Json.required object "rocket_trail"
    antenna <- Json.required object "antenna"
    topper <- Json.required object "topper"
    unknown1 <- Json.required object "unknown1"
    unknown2 <- Json.optional object "unknown2"
    engineAudio <- Json.optional object "engine_audio"
    trail <- Json.optional object "trail"
    goalExplosion <- Json.optional object "goal_explosion"
    banner <- Json.optional object "banner"
    unknown3 <- Json.optional object "unknown3"
    unknown4 <- Json.optional object "unknown4"
    unknown5 <- Json.optional object "unknown5"
    unknown6 <- Json.optional object "unknown6"
    pure Loadout
      { version
      , body
      , decal
      , wheels
      , rocketTrail
      , antenna
      , topper
      , unknown1
      , unknown2
      , engineAudio
      , trail
      , goalExplosion
      , banner
      , unknown3
      , unknown4
      , unknown5
      , unknown6
      }

instance Json.ToJSON Loadout where
  toJSON x = Json.object
    [ Json.pair "version" $ version x
    , Json.pair "body" $ body x
    , Json.pair "decal" $ decal x
    , Json.pair "wheels" $ wheels x
    , Json.pair "rocket_trail" $ rocketTrail x
    , Json.pair "antenna" $ antenna x
    , Json.pair "topper" $ topper x
    , Json.pair "unknown1" $ unknown1 x
    , Json.pair "unknown2" $ unknown2 x
    , Json.pair "engine_audio" $ engineAudio x
    , Json.pair "trail" $ trail x
    , Json.pair "goal_explosion" $ goalExplosion x
    , Json.pair "banner" $ banner x
    , Json.pair "unknown3" $ unknown3 x
    , Json.pair "unknown4" $ unknown4 x
    , Json.pair "unknown5" $ unknown5 x
    , Json.pair "unknown6" $ unknown6 x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-loadout" $ Schema.object
  [ (Json.pair "version" $ Schema.ref U8.schema, True)
  , (Json.pair "body" $ Schema.ref U32.schema, True)
  , (Json.pair "decal" $ Schema.ref U32.schema, True)
  , (Json.pair "wheels" $ Schema.ref U32.schema, True)
  , (Json.pair "rocket_trail" $ Schema.ref U32.schema, True)
  , (Json.pair "antenna" $ Schema.ref U32.schema, True)
  , (Json.pair "topper" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown1" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown2" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "engine_audio" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "trail" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "goal_explosion" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "banner" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "unknown3" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "unknown4" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "unknown5" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "unknown6" . Schema.json $ Schema.maybe U32.schema, False)
  ]

bitPut :: Loadout -> BitPut.BitPut
bitPut loadoutAttribute =
  U8.bitPut (version loadoutAttribute)
    <> U32.bitPut (body loadoutAttribute)
    <> U32.bitPut (decal loadoutAttribute)
    <> U32.bitPut (wheels loadoutAttribute)
    <> U32.bitPut (rocketTrail loadoutAttribute)
    <> U32.bitPut (antenna loadoutAttribute)
    <> U32.bitPut (topper loadoutAttribute)
    <> U32.bitPut (unknown1 loadoutAttribute)
    <> foldMap U32.bitPut (unknown2 loadoutAttribute)
    <> foldMap U32.bitPut (engineAudio loadoutAttribute)
    <> foldMap U32.bitPut (trail loadoutAttribute)
    <> foldMap U32.bitPut (goalExplosion loadoutAttribute)
    <> foldMap U32.bitPut (banner loadoutAttribute)
    <> foldMap U32.bitPut (unknown3 loadoutAttribute)
    <> foldMap U32.bitPut (unknown4 loadoutAttribute)
    <> foldMap U32.bitPut (unknown5 loadoutAttribute)
    <> foldMap U32.bitPut (unknown6 loadoutAttribute)

bitGet :: BitGet.BitGet Loadout
bitGet = do
  version <- U8.bitGet
  body <- U32.bitGet
  decal <- U32.bitGet
  wheels <- U32.bitGet
  rocketTrail <- U32.bitGet
  antenna <- U32.bitGet
  topper <- U32.bitGet
  unknown1 <- U32.bitGet
  unknown2 <- Monad.whenMaybe (U8.toWord8 version >= 11) U32.bitGet
  engineAudio <- Monad.whenMaybe (U8.toWord8 version >= 16) U32.bitGet
  trail <- Monad.whenMaybe (U8.toWord8 version >= 16) U32.bitGet
  goalExplosion <- Monad.whenMaybe (U8.toWord8 version >= 16) U32.bitGet
  banner <- Monad.whenMaybe (U8.toWord8 version >= 17) U32.bitGet
  unknown3 <- Monad.whenMaybe (U8.toWord8 version >= 19) U32.bitGet
  unknown4 <- Monad.whenMaybe (U8.toWord8 version >= 22) U32.bitGet
  unknown5 <- Monad.whenMaybe (U8.toWord8 version >= 22) U32.bitGet
  unknown6 <- Monad.whenMaybe (U8.toWord8 version >= 22) U32.bitGet
  pure Loadout
    { version
    , body
    , decal
    , wheels
    , rocketTrail
    , antenna
    , topper
    , unknown1
    , unknown2
    , engineAudio
    , trail
    , goalExplosion
    , banner
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    }
