module Rattletrap.Decode.QWordAttribute
  ( decodeQWordAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word64le
import Rattletrap.Type.QWordAttribute

decodeQWordAttributeBits :: DecodeBits QWordAttribute
decodeQWordAttributeBits = QWordAttribute <$> decodeWord64leBits
