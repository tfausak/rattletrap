{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data LoadoutAttribute = LoadoutAttribute
  { loadoutAttributeVersion :: Word8le
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
  deriving (Eq, Ord, Show)

$(deriveJson ''LoadoutAttribute)

putLoadoutAttribute :: LoadoutAttribute -> BinaryBits.BitPut ()
putLoadoutAttribute loadoutAttribute = do
  putWord8Bits (loadoutAttributeVersion loadoutAttribute)
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

putOptional :: Maybe a -> (a -> BinaryBits.BitPut ()) -> BinaryBits.BitPut ()
putOptional m f = case m of
  Just x -> f x
  Nothing -> pure ()

decodeLoadoutAttributeBits :: DecodeBits LoadoutAttribute
decodeLoadoutAttributeBits = do
  version <- decodeWord8leBits
  LoadoutAttribute version
    <$> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWhen (version >= Word8le 11) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 16) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 16) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 16) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 17) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 19) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 22) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 22) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 22) decodeWord32leBits
