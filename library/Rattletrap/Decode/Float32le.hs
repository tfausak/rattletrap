module Rattletrap.Decode.Float32le
  ( getFloat32
  , getFloat32Bits
  ) where

import Rattletrap.Type.Float32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary

getFloat32 :: Binary.Get Float32le
getFloat32 = do
  float32 <- Binary.getFloatle
  pure (Float32le float32)

getFloat32Bits :: BinaryBit.BitGet Float32le
getFloat32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getFloat32 (reverseBytes bytes))
