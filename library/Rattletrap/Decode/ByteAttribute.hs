module Rattletrap.Decode.ByteAttribute
  ( getByteAttribute
  ) where

import Rattletrap.Type.ByteAttribute
import Rattletrap.Decode.Word8

import qualified Data.Binary.Bits.Get as BinaryBit

getByteAttribute :: BinaryBit.BitGet ByteAttribute
getByteAttribute = do
  value <- getWord8Bits
  pure (ByteAttribute value)
