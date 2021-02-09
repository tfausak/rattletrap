module Rattletrap.Encode.Word32le
  ( putWord32
  , putWord32Bits
  ) where

import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putWord32 :: Word32le -> Binary.Put
putWord32 word32 = Binary.putWord32le (word32leValue word32)

putWord32Bits :: Word32le -> BinaryBits.BitPut ()
putWord32Bits word32 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putWord32 word32))
  BinaryBits.putByteString (reverseBytes bytes)
