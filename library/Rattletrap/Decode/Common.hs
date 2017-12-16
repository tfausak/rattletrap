module Rattletrap.Decode.Common
  ( Decode
  , DecodeBits
  , toBits
  ) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Binary.Get as Binary
import qualified Rattletrap.Utility.Bytes as Utility

type Decode a = Binary.Get a

type DecodeBits a = BinaryBits.BitGet a

toBits :: Decode a -> Int -> DecodeBits a
toBits decode =
  fmap (Binary.runGet decode . Utility.reverseBytes)
    . BinaryBits.getLazyByteString
