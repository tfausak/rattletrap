{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.MusicStinger where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data MusicStingerAttribute = MusicStingerAttribute
  { musicStingerAttributeFlag :: Bool
  , musicStingerAttributeCue :: Word32le.Word32le
  , musicStingerAttributeTrigger :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''MusicStingerAttribute)

putMusicStingerAttribute :: MusicStingerAttribute -> BitPut ()
putMusicStingerAttribute musicStingerAttribute = do
  BinaryBits.putBool (musicStingerAttributeFlag musicStingerAttribute)
  Word32le.bitPut (musicStingerAttributeCue musicStingerAttribute)
  Word8le.bitPut (musicStingerAttributeTrigger musicStingerAttribute)

decodeMusicStingerAttributeBits :: BitGet MusicStingerAttribute
decodeMusicStingerAttributeBits =
  MusicStingerAttribute
    <$> getBool
    <*> Word32le.bitGet
    <*> Word8le.bitGet
