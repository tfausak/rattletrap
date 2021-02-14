{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.MusicStinger where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data MusicStingerAttribute = MusicStingerAttribute
  { flag :: Bool
  , cue :: Word32le.Word32le
  , trigger :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''MusicStingerAttribute)

bitPut :: MusicStingerAttribute -> BitPut ()
bitPut musicStingerAttribute = do
  BinaryBits.putBool (flag musicStingerAttribute)
  Word32le.bitPut (cue musicStingerAttribute)
  Word8le.bitPut (trigger musicStingerAttribute)

bitGet :: BitGet MusicStingerAttribute
bitGet =
  MusicStingerAttribute
    <$> getBool
    <*> Word32le.bitGet
    <*> Word8le.bitGet
