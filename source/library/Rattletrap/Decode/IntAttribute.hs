module Rattletrap.Decode.IntAttribute
  ( decodeIntAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.IntAttribute

decodeIntAttributeBits :: DecodeBits IntAttribute
decodeIntAttributeBits = IntAttribute <$> decodeInt32leBits
