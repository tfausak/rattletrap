module Rattletrap.Decode.Int8le
  ( getInt8
  , getInt8Bits
  ) where

import Rattletrap.Type.Int8le
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getInt8 :: Binary.Get Int8le
getInt8 = do
  int8 <- Binary.getInt8
  pure (Int8le int8)

getInt8Bits :: BinaryBit.BitGet Int8le
getInt8Bits = do
  bytes <- BinaryBit.getLazyByteString 1
  pure (Binary.runGet getInt8 (reverseBytes bytes))
