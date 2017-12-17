module Rattletrap.Decode.MusicStingerAttribute
  ( decodeMusicStingerAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.MusicStingerAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeMusicStingerAttributeBits :: DecodeBits MusicStingerAttribute
decodeMusicStingerAttributeBits =
  MusicStingerAttribute
    <$> BinaryBits.getBool
    <*> decodeWord32leBits
    <*> decodeWord8leBits
