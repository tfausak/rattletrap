module Rattletrap.Decode.Word64le
  ( decodeWord64le
  , decodeWord64leBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.Word64le

import qualified Data.Binary.Get as Binary

decodeWord64le :: Decode Word64le
decodeWord64le = Word64le <$> Binary.getWord64le

decodeWord64leBits :: DecodeBits Word64le
decodeWord64leBits = toBits decodeWord64le 8
