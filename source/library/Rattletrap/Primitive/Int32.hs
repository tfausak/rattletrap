module Rattletrap.Primitive.Int32 where

import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Int as Int

newtype Int32 = Int32
  { int32Value :: Int.Int32
  } deriving (Eq, Show)

getInt32 :: Binary.Get Int32
getInt32 = do
  int32 <- Binary.getInt32le
  pure (Int32 int32)

putInt32 :: Int32 -> Binary.Put
putInt32 int32 = Binary.putInt32le (int32Value int32)

getInt32Bits :: BinaryBit.BitGet Int32
getInt32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getInt32 (reverseBytes bytes))

putInt32Bits :: Int32 -> BinaryBit.BitPut ()
putInt32Bits int32 = do
  let bytes = Binary.runPut (putInt32 int32)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
