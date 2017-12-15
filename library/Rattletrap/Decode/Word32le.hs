module Rattletrap.Decode.Word32le
  ( getWord32
  , getWord32Bits
  ) where

import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getWord32 :: Binary.Get Word32le
getWord32 = do
  word32 <- Binary.getWord32le
  pure (Word32le word32)

getWord32Bits :: BinaryBit.BitGet Word32le
getWord32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getWord32 (reverseBytes bytes))
