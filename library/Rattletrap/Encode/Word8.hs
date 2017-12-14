module Rattletrap.Encode.Word8
  ( putWord8
  , putWord8Bits
  ) where

import Rattletrap.Type.Word8
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

putWord8 :: Word8 -> Binary.Put
putWord8 word8 = Binary.putWord8 (word8Value word8)

putWord8Bits :: Word8 -> BinaryBit.BitPut ()
putWord8Bits word8 = do
  let bytes = Binary.runPut (putWord8 word8)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
