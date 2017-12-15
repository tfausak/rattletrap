module Rattletrap.Decode.Int32le
  ( getInt32
  , getInt32Bits
  ) where

import Rattletrap.Type.Int32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getInt32 :: Binary.Get Int32le
getInt32 = do
  int32 <- Binary.getInt32le
  pure (Int32le int32)

getInt32Bits :: BinaryBit.BitGet Int32le
getInt32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getInt32 (reverseBytes bytes))
