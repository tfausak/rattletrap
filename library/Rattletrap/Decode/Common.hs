module Rattletrap.Decode.Common
  ( Decode
  , DecodeBits
  , decodeWhen
  , toBits
  ) where

import qualified Control.Applicative as Applicative
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Binary.Get as Binary
import qualified Rattletrap.Utility.Bytes as Utility

type Decode a = Binary.Get a

type DecodeBits a = BinaryBits.BitGet a

decodeWhen
  :: (Applicative m, Applicative.Alternative f) => Bool -> m a -> m (f a)
decodeWhen p f = if p then fmap pure f else pure Applicative.empty

toBits :: Decode a -> Int -> DecodeBits a
toBits decode =
  fmap (Binary.runGet decode . Utility.reverseBytes)
    . BinaryBits.getLazyByteString
