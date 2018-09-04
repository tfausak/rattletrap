module Rattletrap.Decode.LoadoutAttribute
  ( decodeLoadoutAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.LoadoutAttribute
import Rattletrap.Type.Word8le

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
    <*> decodeWhen (version > Word8le 10) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 16) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 16) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 16) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 17) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 19) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 22) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 22) decodeWord32leBits
    <*> decodeWhen (version >= Word8le 22) decodeWord32leBits
