module Rattletrap.Encode.Word32le
  ( putWord32
  , putWord32Bits
  ) where

import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

putWord32 :: Word32le -> Binary.Put
putWord32 word32 = Binary.putWord32le (word32leValue word32)

putWord32Bits :: Word32le -> BinaryBit.BitPut ()
putWord32Bits word32 = do
  let bytes = Binary.runPut (putWord32 word32)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
