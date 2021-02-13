{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.MusicStingerAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data MusicStingerAttribute = MusicStingerAttribute
  { musicStingerAttributeFlag :: Bool
  , musicStingerAttributeCue :: Word32le
  , musicStingerAttributeTrigger :: Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''MusicStingerAttribute)

putMusicStingerAttribute :: MusicStingerAttribute -> BinaryBits.BitPut ()
putMusicStingerAttribute musicStingerAttribute = do
  BinaryBits.putBool (musicStingerAttributeFlag musicStingerAttribute)
  putWord32Bits (musicStingerAttributeCue musicStingerAttribute)
  putWord8Bits (musicStingerAttributeTrigger musicStingerAttribute)

decodeMusicStingerAttributeBits :: DecodeBits MusicStingerAttribute
decodeMusicStingerAttributeBits =
  MusicStingerAttribute
    <$> getBool
    <*> decodeWord32leBits
    <*> decodeWord8leBits
