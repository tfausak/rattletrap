{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadout where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data LoadoutAttribute = LoadoutAttribute
  { loadoutAttributeVersion :: Word8le.Word8le
  , loadoutAttributeBody :: Word32le.Word32le
  , loadoutAttributeDecal :: Word32le.Word32le
  , loadoutAttributeWheels :: Word32le.Word32le
  , loadoutAttributeRocketTrail :: Word32le.Word32le
  -- ^ Now known as "rocket boost".
  , loadoutAttributeAntenna :: Word32le.Word32le
  , loadoutAttributeTopper :: Word32le.Word32le
  , loadoutAttributeUnknown1 :: Word32le.Word32le
  , loadoutAttributeUnknown2 :: Maybe Word32le.Word32le
  , loadoutAttributeEngineAudio :: Maybe Word32le.Word32le
  , loadoutAttributeTrail :: Maybe Word32le.Word32le
  , loadoutAttributeGoalExplosion :: Maybe Word32le.Word32le
  , loadoutAttributeBanner :: Maybe Word32le.Word32le
  , loadoutAttributeUnknown3 :: Maybe Word32le.Word32le
  , loadoutAttributeUnknown4 :: Maybe Word32le.Word32le
  , loadoutAttributeUnknown5 :: Maybe Word32le.Word32le
  , loadoutAttributeUnknown6 :: Maybe Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutAttribute)

putLoadoutAttribute :: LoadoutAttribute -> BitPut ()
putLoadoutAttribute loadoutAttribute = do
  Word8le.bitPut (loadoutAttributeVersion loadoutAttribute)
  Word32le.bitPut (loadoutAttributeBody loadoutAttribute)
  Word32le.bitPut (loadoutAttributeDecal loadoutAttribute)
  Word32le.bitPut (loadoutAttributeWheels loadoutAttribute)
  Word32le.bitPut (loadoutAttributeRocketTrail loadoutAttribute)
  Word32le.bitPut (loadoutAttributeAntenna loadoutAttribute)
  Word32le.bitPut (loadoutAttributeTopper loadoutAttribute)
  Word32le.bitPut (loadoutAttributeUnknown1 loadoutAttribute)
  putOptional (loadoutAttributeUnknown2 loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeEngineAudio loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeTrail loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeGoalExplosion loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeBanner loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeUnknown3 loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeUnknown4 loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeUnknown5 loadoutAttribute) Word32le.bitPut
  putOptional (loadoutAttributeUnknown6 loadoutAttribute) Word32le.bitPut

putOptional :: Maybe a -> (a -> BitPut ()) -> BitPut ()
putOptional m f = case m of
  Just x -> f x
  Nothing -> pure ()

decodeLoadoutAttributeBits :: BitGet LoadoutAttribute
decodeLoadoutAttributeBits = do
  version <- Word8le.bitGet
  LoadoutAttribute version
    <$> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 11) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 16) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 16) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 16) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 17) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 19) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 22) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 22) Word32le.bitGet
    <*> decodeWhen (Word8le.toWord8 version >= 22) Word32le.bitGet
