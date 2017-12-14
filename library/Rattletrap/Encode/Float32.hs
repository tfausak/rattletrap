module Rattletrap.Encode.Float32
  ( putFloat32
  , putFloat32Bits
  ) where

import Rattletrap.Type.Float32
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

putFloat32 :: Float32 -> Binary.Put
putFloat32 = IEEE754.putFloat32le . float32Value

putFloat32Bits :: Float32 -> BinaryBit.BitPut ()
putFloat32Bits float32 = do
  let bytes = Binary.runPut (putFloat32 float32)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
