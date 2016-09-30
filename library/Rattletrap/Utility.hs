module Rattletrap.Utility where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Word as Word

logBase2
  :: (Integral a, Integral b)
  => a -> b
logBase2 x = floor (logBase (2 :: Float) (fromIntegral x))

padLazyByteString
  :: Integral a
  => a -> LazyByteString.ByteString -> LazyByteString.ByteString
padLazyByteString size bytes =
  LazyByteString.concat
    [ bytes
    , LazyByteString.replicate
        (fromIntegral size - LazyByteString.length bytes)
        0x00
    ]

reverseByte :: Word.Word8 -> Word.Word8
reverseByte byte =
  Bits.shiftR (byte Bits..&. Bits.bit 8) 7 +
  Bits.shiftR (byte Bits..&. Bits.bit 7) 5 +
  Bits.shiftR (byte Bits..&. Bits.bit 6) 3 +
  Bits.shiftR (byte Bits..&. Bits.bit 5) 1 +
  Bits.shiftL (byte Bits..&. Bits.bit 4) 1 +
  Bits.shiftL (byte Bits..&. Bits.bit 3) 3 +
  Bits.shiftL (byte Bits..&. Bits.bit 2) 5 +
  Bits.shiftL (byte Bits..&. Bits.bit 1) 7

reverseLazyByteString :: LazyByteString.ByteString -> LazyByteString.ByteString
reverseLazyByteString = LazyByteString.map reverseByte
