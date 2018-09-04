module Rattletrap.Encode.Word64le
  ( putWord64
  , putWord64Bits
  )
where

import Rattletrap.Type.Word64le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putWord64 :: Word64le -> Binary.Put
putWord64 word64 = Binary.putWord64le (word64leValue word64)

putWord64Bits :: Word64le -> BinaryBits.BitPut ()
putWord64Bits word64 = do
  let bytes = Binary.runPut (putWord64 word64)
  BinaryBits.putByteString (LazyBytes.toStrict (reverseBytes bytes))
