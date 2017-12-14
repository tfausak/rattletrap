module Rattletrap.Decode.Word8
  ( getWord8
  , getWord8Bits
  ) where

import Rattletrap.Type.Word8
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getWord8 :: Binary.Get Word8
getWord8 = do
  word8 <- Binary.getWord8
  pure (Word8 word8)

getWord8Bits :: BinaryBit.BitGet Word8
getWord8Bits = do
  bytes <- BinaryBit.getLazyByteString 1
  pure (Binary.runGet getWord8 (reverseBytes bytes))
