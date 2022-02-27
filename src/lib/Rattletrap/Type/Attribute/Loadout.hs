module Rattletrap.Type.Attribute.Loadout where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

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

instance Argo.HasCodec Loadout where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ Loadout
      <$> Argo.required version "version"
      <*> Argo.required body "body"
      <*> Argo.required decal "decal"
      <*> Argo.required wheels "wheels"
      <*> Argo.required rocketTrail "rocket_trail"
      <*> Argo.required antenna "antenna"
      <*> Argo.required topper "topper"
      <*> Argo.required unknown1 "unknown1"
      <*> Argo.optional unknown2 "unknown2"
      <*> Argo.optional engineAudio "engine_audio"
      <*> Argo.optional trail "trail"
      <*> Argo.optional goalExplosion "goal_explosion"
      <*> Argo.optional banner "banner"
      <*> Argo.optional unknown3 "unknown3"
      <*> Argo.optional unknown4 "unknown4"
      <*> Argo.optional unknown5 "unknown5"
      <*> Argo.optional unknown6 "unknown6"

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
bitGet = BitGet.label "Loadout" $ do
  version <- BitGet.label "version" U8.bitGet
  body <- BitGet.label "body" U32.bitGet
  decal <- BitGet.label "decal" U32.bitGet
  wheels <- BitGet.label "wheels" U32.bitGet
  rocketTrail <- BitGet.label "rocketTrail" U32.bitGet
  antenna <- BitGet.label "antenna" U32.bitGet
  topper <- BitGet.label "topper" U32.bitGet
  unknown1 <- BitGet.label "unknown1" U32.bitGet
  unknown2 <- BitGet.label "unknown2"
    $ Monad.whenMaybe (U8.toWord8 version >= 11) U32.bitGet
  engineAudio <- BitGet.label "engineAudio"
    $ Monad.whenMaybe (U8.toWord8 version >= 16) U32.bitGet
  trail <- BitGet.label "trail"
    $ Monad.whenMaybe (U8.toWord8 version >= 16) U32.bitGet
  goalExplosion <- BitGet.label "goalExplosion"
    $ Monad.whenMaybe (U8.toWord8 version >= 16) U32.bitGet
  banner <- BitGet.label "banner"
    $ Monad.whenMaybe (U8.toWord8 version >= 17) U32.bitGet
  unknown3 <- BitGet.label "unknown3"
    $ Monad.whenMaybe (U8.toWord8 version >= 19) U32.bitGet
  unknown4 <- BitGet.label "unknown4"
    $ Monad.whenMaybe (U8.toWord8 version >= 22) U32.bitGet
  unknown5 <- BitGet.label "unknown5"
    $ Monad.whenMaybe (U8.toWord8 version >= 22) U32.bitGet
  unknown6 <- BitGet.label "unknown6"
    $ Monad.whenMaybe (U8.toWord8 version >= 22) U32.bitGet
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
