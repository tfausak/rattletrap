{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadout where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Utility.Monad

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

$(deriveJson ''Loadout)

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
  version_ <- U8.bitGet
  Loadout version_
    <$> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 11) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 16) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 16) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 16) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 17) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 19) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 22) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 22) U32.bitGet
    <*> whenMaybe (U8.toWord8 version_ >= 22) U32.bitGet
