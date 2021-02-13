{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadout where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data LoadoutAttribute = LoadoutAttribute
  { loadoutAttributeVersion :: Word8le.Word8le
  , loadoutAttributeBody :: Word32le
  , loadoutAttributeDecal :: Word32le
  , loadoutAttributeWheels :: Word32le
  , loadoutAttributeRocketTrail :: Word32le
  -- ^ Now known as "rocket boost".
  , loadoutAttributeAntenna :: Word32le
  , loadoutAttributeTopper :: Word32le
  , loadoutAttributeUnknown1 :: Word32le
  , loadoutAttributeUnknown2 :: Maybe Word32le
  , loadoutAttributeEngineAudio :: Maybe Word32le
  , loadoutAttributeTrail :: Maybe Word32le
  , loadoutAttributeGoalExplosion :: Maybe Word32le
  , loadoutAttributeBanner :: Maybe Word32le
  , loadoutAttributeUnknown3 :: Maybe Word32le
  , loadoutAttributeUnknown4 :: Maybe Word32le
  , loadoutAttributeUnknown5 :: Maybe Word32le
  , loadoutAttributeUnknown6 :: Maybe Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutAttribute)

putLoadoutAttribute :: LoadoutAttribute -> BitPut ()
putLoadoutAttribute loadoutAttribute = do
  Word8le.bitPut (loadoutAttributeVersion loadoutAttribute)
  putWord32Bits (loadoutAttributeBody loadoutAttribute)
  putWord32Bits (loadoutAttributeDecal loadoutAttribute)
  putWord32Bits (loadoutAttributeWheels loadoutAttribute)
  putWord32Bits (loadoutAttributeRocketTrail loadoutAttribute)
  putWord32Bits (loadoutAttributeAntenna loadoutAttribute)
  putWord32Bits (loadoutAttributeTopper loadoutAttribute)
  putWord32Bits (loadoutAttributeUnknown1 loadoutAttribute)
  putOptional (loadoutAttributeUnknown2 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeEngineAudio loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeTrail loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeGoalExplosion loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeBanner loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeUnknown3 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeUnknown4 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeUnknown5 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeUnknown6 loadoutAttribute) putWord32Bits

putOptional :: Maybe a -> (a -> BitPut ()) -> BitPut ()
putOptional m f = case m of
  Just x -> f x
  Nothing -> pure ()

decodeLoadoutAttributeBits :: BitGet LoadoutAttribute
decodeLoadoutAttributeBits = do
  version <- Word8le.bitGet
  LoadoutAttribute version
    <$> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 11) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 16) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 16) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 16) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 17) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 19) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 22) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 22) decodeWord32leBits
    <*> decodeWhen (Word8le.toWord8 version >= 22) decodeWord32leBits
