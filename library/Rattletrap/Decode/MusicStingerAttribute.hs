module Rattletrap.Decode.MusicStingerAttribute
  ( decodeMusicStingerAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.MusicStingerAttribute

decodeMusicStingerAttributeBits :: DecodeBits MusicStingerAttribute
decodeMusicStingerAttributeBits =
  MusicStingerAttribute
    <$> getBool
    <*> decodeWord32leBits
    <*> decodeWord8leBits
