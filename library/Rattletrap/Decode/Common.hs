module Rattletrap.Decode.Common
  ( Decode
  , DecodeBits
  ) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBits

type Decode a = Binary.Get a

type DecodeBits a = BinaryBits.BitGet a
