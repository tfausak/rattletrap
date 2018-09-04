module Rattletrap.Utility.Bytes
  ( encodeLatin1
  , padBytes
  , reverseByte
  , reverseBytes
  )
where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.ByteString.Lazy.Char8 as LazyBytes8
import qualified Data.Text as Text
import qualified Data.Word as Word

encodeLatin1 :: Text.Text -> LazyBytes.ByteString
encodeLatin1 text = LazyBytes8.pack (Text.unpack text)

padBytes :: Integral a => a -> LazyBytes.ByteString -> LazyBytes.ByteString
padBytes size bytes = LazyBytes.concat
  [ bytes
  , LazyBytes.replicate (fromIntegral size - LazyBytes.length bytes) 0x00
  ]

reverseByte :: Word.Word8 -> Word.Word8
reverseByte byte =
  Bits.shiftR (byte Bits..&. Bits.bit 7) 7
    + Bits.shiftR (byte Bits..&. Bits.bit 6) 5
    + Bits.shiftR (byte Bits..&. Bits.bit 5) 3
    + Bits.shiftR (byte Bits..&. Bits.bit 4) 1
    + Bits.shiftL (byte Bits..&. Bits.bit 3) 1
    + Bits.shiftL (byte Bits..&. Bits.bit 2) 3
    + Bits.shiftL (byte Bits..&. Bits.bit 1) 5
    + Bits.shiftL (byte Bits..&. Bits.bit 0) 7

reverseBytes :: LazyBytes.ByteString -> LazyBytes.ByteString
reverseBytes = LazyBytes.map reverseByte
