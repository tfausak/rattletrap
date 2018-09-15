module Rattletrap.Utility.Bytes
  ( encodeLatin1
  , padBytes
  , reverseByte
  , reverseBytes
  )
where

import qualified Data.Bits as Bits
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Bytes8
import qualified Data.Text as Text
import qualified Data.Word as Word

encodeLatin1 :: Text.Text -> Bytes.ByteString
encodeLatin1 text = Bytes8.pack (Text.unpack text)

padBytes :: Integral a => a -> Bytes.ByteString -> Bytes.ByteString
padBytes size bytes =
  bytes <> Bytes.replicate (fromIntegral size - Bytes.length bytes) 0x00

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

reverseBytes :: Bytes.ByteString -> Bytes.ByteString
reverseBytes = Bytes.map reverseByte
