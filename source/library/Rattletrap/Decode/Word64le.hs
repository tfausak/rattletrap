module Rattletrap.Decode.Word64le
  ( decodeWord64le
  , decodeWord64leBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.Word64le

decodeWord64le :: Decode Word64le
decodeWord64le = Word64le <$> getWord64le

decodeWord64leBits :: DecodeBits Word64le
decodeWord64leBits = toBits decodeWord64le 8
