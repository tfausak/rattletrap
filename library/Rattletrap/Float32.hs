module Rattletrap.Float32 where

import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

newtype Float32 = Float32
  { float32Value :: Float
  } deriving (Eq, Ord, Show)

getFloat32 :: Binary.Get Float32
getFloat32 = do
  float32 <- IEEE754.getFloat32le
  pure (Float32 float32)

putFloat32 :: Float32 -> Binary.Put
putFloat32 float32 = IEEE754.putFloat32le (float32Value float32)

getFloat32Bits :: BinaryBit.BitGet Float32
getFloat32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getFloat32 (reverseBytes bytes))

putFloat32Bits :: Float32 -> BinaryBit.BitPut ()
putFloat32Bits float32 = do
  let bytes = Binary.runPut (putFloat32 float32)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
