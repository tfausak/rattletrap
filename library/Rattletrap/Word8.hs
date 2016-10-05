module Rattletrap.Word8 where

import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Word as Word

newtype Word8 = Word8
  { word8Value :: Word.Word8
  } deriving (Eq, Ord, Show)

getWord8 :: Binary.Get Word8
getWord8 = do
  word8 <- Binary.getWord8
  pure (Word8 word8)

putWord8 :: Word8 -> Binary.Put
putWord8 (Word8 word8) = Binary.putWord8 word8

getWord8Bits :: BinaryBit.BitGet Word8
getWord8Bits = do
  bytes <- BinaryBit.getLazyByteString 1
  pure (Binary.runGet getWord8 (reverseBytes bytes))

putWord8Bits :: Word8 -> BinaryBit.BitPut ()
putWord8Bits word8 = do
  let bytes = Binary.runPut (putWord8 word8)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
