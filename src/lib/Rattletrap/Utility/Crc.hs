module Rattletrap.Utility.Crc
  ( getCrc32
  ) where

import Rattletrap.Data

import qualified Data.Array.Unboxed as Array
import qualified Data.Bits as Bits
import qualified Data.ByteString as Bytes
import qualified Data.Word as Word

-- | Computes the CRC32 of some bytes. This is done to ensure that the bytes
-- are valid before trying to parse them.
--
-- @
-- getCrc32 ('Data.ByteString.Lazy.pack' [0x00])
-- @
--
-- This CRC uses an initial value of @0xefcbf201@ and a polynomial of
-- @0x04c11db7@.
getCrc32 :: Bytes.ByteString -> Word.Word32
getCrc32 bytes = do
  let
    update = crc32Update crc32Table
    initial = Bits.complement crc32Initial
    crc = Bytes.foldl' update initial bytes
  Bits.complement crc

crc32Update
  :: Array.Array Word.Word8 Word.Word32 -> Word.Word32 -> Word.Word8 -> Word.Word32
crc32Update table crc byte = do
  let
    toWord8 :: (Integral a) => a -> Word.Word8
    toWord8 = fromIntegral
    index = Bits.xor byte (toWord8 (Bits.shiftR crc 24))
    left = table Array.! index
    right = Bits.shiftL crc 8
  Bits.xor left right

crc32Initial :: Word.Word32
crc32Initial = 0xefcbf201
