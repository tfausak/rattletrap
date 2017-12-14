module Rattletrap.Encode.MusicStingerAttribute
  ( putMusicStingerAttribute
  ) where

import Rattletrap.Type.MusicStingerAttribute
import Rattletrap.Encode.Word32
import Rattletrap.Encode.Word8

import qualified Data.Binary.Bits.Put as BinaryBit

putMusicStingerAttribute :: MusicStingerAttribute -> BinaryBit.BitPut ()
putMusicStingerAttribute musicStingerAttribute = do
  BinaryBit.putBool (musicStingerAttributeFlag musicStingerAttribute)
  putWord32Bits (musicStingerAttributeCue musicStingerAttribute)
  putWord8Bits (musicStingerAttributeTrigger musicStingerAttribute)
