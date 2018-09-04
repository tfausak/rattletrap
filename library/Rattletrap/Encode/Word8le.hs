module Rattletrap.Encode.Word8le
  ( putWord8
  , putWord8Bits
  )
where

import Rattletrap.Type.Word8le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putWord8 :: Word8le -> Binary.Put
putWord8 word8 = Binary.putWord8 (word8leValue word8)

putWord8Bits :: Word8le -> BinaryBits.BitPut ()
putWord8Bits word8 = do
  let bytes = Binary.runPut (putWord8 word8)
  BinaryBits.putByteString (LazyBytes.toStrict (reverseBytes bytes))
