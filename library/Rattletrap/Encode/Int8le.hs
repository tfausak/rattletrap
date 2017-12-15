module Rattletrap.Encode.Int8le
  ( putInt8
  , putInt8Bits
  ) where

import Rattletrap.Type.Int8le
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

putInt8 :: Int8le -> Binary.Put
putInt8 int8 = Binary.putInt8 (int8Value int8)

putInt8Bits :: Int8le -> BinaryBit.BitPut ()
putInt8Bits int8 = do
  let bytes = Binary.runPut (putInt8 int8)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
