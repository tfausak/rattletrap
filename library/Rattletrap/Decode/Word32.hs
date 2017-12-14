module Rattletrap.Decode.Word32
  ( getWord32
  , getWord32Bits
  ) where

import Rattletrap.Type.Word32
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getWord32 :: Binary.Get Word32
getWord32 = do
  word32 <- Binary.getWord32le
  pure (Word32 word32)

getWord32Bits :: BinaryBit.BitGet Word32
getWord32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getWord32 (reverseBytes bytes))
