module Rattletrap.Decode.Word8le
  ( decodeWord8le
  , decodeWord8leBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.Word8le

decodeWord8le :: Decode Word8le
decodeWord8le = Word8le <$> getWord8

decodeWord8leBits :: DecodeBits Word8le
decodeWord8leBits = toBits decodeWord8le 1
