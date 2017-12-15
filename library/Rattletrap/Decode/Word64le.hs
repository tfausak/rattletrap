module Rattletrap.Decode.Word64le
  ( getWord64
  , getWord64Bits
  ) where

import Rattletrap.Type.Word64le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getWord64 :: Binary.Get Word64le
getWord64 = do
  word64 <- Binary.getWord64le
  pure (Word64le word64)

getWord64Bits :: BinaryBit.BitGet Word64le
getWord64Bits = do
  bytes <- BinaryBit.getLazyByteString 8
  pure (Binary.runGet getWord64 (reverseBytes bytes))
