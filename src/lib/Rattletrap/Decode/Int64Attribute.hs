module Rattletrap.Decode.Int64Attribute
  ( decodeInt64AttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int64le
import Rattletrap.Type.Int64Attribute

decodeInt64AttributeBits :: DecodeBits Int64Attribute
decodeInt64AttributeBits = Int64Attribute <$> decodeInt64leBits
