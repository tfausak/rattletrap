module Rattletrap.Encode.Word64
  ( putWord64
  , putWord64Bits
  ) where

import Rattletrap.Type.Word64
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

putWord64 :: Word64 -> Binary.Put
putWord64 word64 = Binary.putWord64le (word64Value word64)

putWord64Bits :: Word64 -> BinaryBit.BitPut ()
putWord64Bits word64 = do
  let bytes = Binary.runPut (putWord64 word64)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
