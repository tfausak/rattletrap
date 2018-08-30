module Rattletrap.Decode.Int64Attribute
  ( decodeIntAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int64le
import Rattletrap.Type.IntAttribute

decodeIntAttributeBits :: DecodeBits IntAttribute
decodeIntAttributeBits = IntAttribute <$> decodeInt64leBits
