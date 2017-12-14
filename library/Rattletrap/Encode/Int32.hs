module Rattletrap.Encode.Int32
  ( putInt32
  , putInt32Bits
  ) where

import Rattletrap.Type.Int32
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

putInt32 :: Int32 -> Binary.Put
putInt32 int32 = Binary.putInt32le (int32Value int32)

putInt32Bits :: Int32 -> BinaryBit.BitPut ()
putInt32Bits int32 = do
  let bytes = Binary.runPut (putInt32 int32)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
