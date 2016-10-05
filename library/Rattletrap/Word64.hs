module Rattletrap.Word64 where

import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Word as Word

newtype Word64 = Word64
  { word64Value :: Word.Word64
  } deriving (Eq, Ord, Show)

getWord64 :: Binary.Get Word64
getWord64 = do
  word64 <- Binary.getWord64le
  pure (Word64 word64)

putWord64 :: Word64 -> Binary.Put
putWord64 (Word64 word64) = Binary.putWord64le word64

getWord64Bits :: BinaryBit.BitGet Word64
getWord64Bits = do
  bytes <- BinaryBit.getLazyByteString 8
  pure (Binary.runGet getWord64 (reverseBytes bytes))

putWord64Bits :: Word64 -> BinaryBit.BitPut ()
putWord64Bits word64 = do
  let bytes = Binary.runPut (putWord64 word64)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
