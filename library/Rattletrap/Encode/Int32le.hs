module Rattletrap.Encode.Int32le
  ( putInt32
  , putInt32Bits
  )
where

import Rattletrap.Type.Int32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putInt32 :: Int32le -> Binary.Put
putInt32 int32 = Binary.putInt32le (int32leValue int32)

putInt32Bits :: Int32le -> BinaryBits.BitPut ()
putInt32Bits int32 = do
  let bytes = Binary.runPut (putInt32 int32)
  BinaryBits.putByteString (LazyBytes.toStrict (reverseBytes bytes))
