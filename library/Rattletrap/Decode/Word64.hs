module Rattletrap.Decode.Word64
  ( getWord64
  , getWord64Bits
  ) where

import Rattletrap.Type.Word64
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getWord64 :: Binary.Get Word64
getWord64 = do
  word64 <- Binary.getWord64le
  pure (Word64 word64)

getWord64Bits :: BinaryBit.BitGet Word64
getWord64Bits = do
  bytes <- BinaryBit.getLazyByteString 8
  pure (Binary.runGet getWord64 (reverseBytes bytes))
