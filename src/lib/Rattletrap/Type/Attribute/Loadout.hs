{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadout where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data LoadoutAttribute = LoadoutAttribute
  { version :: Word8le.Word8le
  , body :: Word32le.Word32le
  , decal :: Word32le.Word32le
  , wheels :: Word32le.Word32le
  , rocketTrail :: Word32le.Word32le
  -- ^ Now known as "rocket boost".
  , antenna :: Word32le.Word32le
  , topper :: Word32le.Word32le
  , unknown1 :: Word32le.Word32le
  , unknown2 :: Maybe Word32le.Word32le
  , engineAudio :: Maybe Word32le.Word32le
  , trail :: Maybe Word32le.Word32le
  , goalExplosion :: Maybe Word32le.Word32le
  , banner :: Maybe Word32le.Word32le
  , unknown3 :: Maybe Word32le.Word32le
  , unknown4 :: Maybe Word32le.Word32le
  , unknown5 :: Maybe Word32le.Word32le
  , unknown6 :: Maybe Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJsonWith ''LoadoutAttribute jsonOptions)

bitPut :: LoadoutAttribute -> BitPut ()
bitPut loadoutAttribute = do
  Word8le.bitPut (version loadoutAttribute)
  Word32le.bitPut (body loadoutAttribute)
  Word32le.bitPut (decal loadoutAttribute)
  Word32le.bitPut (wheels loadoutAttribute)
  Word32le.bitPut (rocketTrail loadoutAttribute)
  Word32le.bitPut (antenna loadoutAttribute)
  Word32le.bitPut (topper loadoutAttribute)
  Word32le.bitPut (unknown1 loadoutAttribute)
  putOptional (unknown2 loadoutAttribute) Word32le.bitPut
  putOptional (engineAudio loadoutAttribute) Word32le.bitPut
  putOptional (trail loadoutAttribute) Word32le.bitPut
  putOptional (goalExplosion loadoutAttribute) Word32le.bitPut
  putOptional (banner loadoutAttribute) Word32le.bitPut
  putOptional (unknown3 loadoutAttribute) Word32le.bitPut
  putOptional (unknown4 loadoutAttribute) Word32le.bitPut
  putOptional (unknown5 loadoutAttribute) Word32le.bitPut
  putOptional (unknown6 loadoutAttribute) Word32le.bitPut

putOptional :: Maybe a -> (a -> BitPut ()) -> BitPut ()
putOptional m f = case m of
  Just x -> f x
  Nothing -> pure ()

bitGet :: BitGet LoadoutAttribute
bitGet = do
  version_ <- Word8le.bitGet
  LoadoutAttribute version_
    <$> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 11) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 16) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 16) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 16) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 17) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 19) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 22) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 22) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version_ >= 22) Word32le.bitGet
