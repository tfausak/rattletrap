module Rattletrap.Encode.MusicStingerAttribute
  ( putMusicStingerAttribute
  ) where

import Rattletrap.Type.Word32le
import Rattletrap.Encode.Word8le
import Rattletrap.Type.MusicStingerAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putMusicStingerAttribute :: MusicStingerAttribute -> BinaryBits.BitPut ()
putMusicStingerAttribute musicStingerAttribute = do
  BinaryBits.putBool (musicStingerAttributeFlag musicStingerAttribute)
  putWord32Bits (musicStingerAttributeCue musicStingerAttribute)
  putWord8Bits (musicStingerAttributeTrigger musicStingerAttribute)
