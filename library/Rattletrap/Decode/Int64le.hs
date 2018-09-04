module Rattletrap.Decode.Int64le
  ( decodeInt64le
  , decodeInt64leBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.Int64le

decodeInt64le :: Decode Int64le
decodeInt64le = Int64le <$> getInt64le

decodeInt64leBits :: DecodeBits Int64le
decodeInt64leBits = toBits decodeInt64le 8
