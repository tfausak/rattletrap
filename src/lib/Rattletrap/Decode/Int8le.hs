module Rattletrap.Decode.Int8le
  ( decodeInt8leBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.Int8le

decodeInt8le :: Decode Int8le
decodeInt8le = Int8le <$> getInt8

decodeInt8leBits :: DecodeBits Int8le
decodeInt8leBits = toBits decodeInt8le 1
