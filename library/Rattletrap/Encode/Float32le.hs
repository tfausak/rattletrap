module Rattletrap.Encode.Float32le
  ( putFloat32
  , putFloat32Bits
  )
where

import Rattletrap.Type.Float32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putFloat32 :: Float32le -> Binary.Put
putFloat32 = Binary.putFloatle . float32leValue

putFloat32Bits :: Float32le -> BinaryBits.BitPut ()
putFloat32Bits float32 = do
  let bytes = Binary.runPut (putFloat32 float32)
  BinaryBits.putByteString (LazyBytes.toStrict (reverseBytes bytes))
