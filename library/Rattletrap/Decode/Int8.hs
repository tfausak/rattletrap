module Rattletrap.Decode.Int8
  ( getInt8
  , getInt8Bits
  ) where

import Rattletrap.Type.Int8
import Rattletrap.Utility

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getInt8 :: Binary.Get Int8
getInt8 = do
  int8 <- Binary.getInt8
  pure (Int8 int8)

getInt8Bits :: BinaryBit.BitGet Int8
getInt8Bits = do
  bytes <- BinaryBit.getLazyByteString 1
  pure (Binary.runGet getInt8 (reverseBytes bytes))
