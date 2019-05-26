module Rattletrap.Encode.Int64le
  ( putInt64Bits
  )
where

import Rattletrap.Type.Int64le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putInt64 :: Int64le -> Binary.Put
putInt64 int64 = Binary.putInt64le (int64leValue int64)

putInt64Bits :: Int64le -> BinaryBits.BitPut ()
putInt64Bits int64 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putInt64 int64))
  BinaryBits.putByteString (reverseBytes bytes)
