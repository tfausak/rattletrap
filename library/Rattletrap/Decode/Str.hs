module Rattletrap.Decode.Str
  ( getText
  , getTextBits
  ) where

import Rattletrap.Decode.Int32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

getText :: Binary.Get Str
getText = do
  rawSize <- getInt32
  let decode = getTextDecoder rawSize
  let size = normalizeTextSize rawSize :: Int64
  bytes <- Binary.getLazyByteString size
  let text = dropNull (decode bytes)
  pure (Str text)

getTextBits :: BinaryBit.BitGet Str
getTextBits = do
  rawSize <- getInt32Bits
  let decode = getTextDecoder rawSize
  let size = normalizeTextSize rawSize :: Int
  bytes <- BinaryBit.getLazyByteString size
  let text = dropNull (decode (reverseBytes bytes))
  pure (Str text)

normalizeTextSize :: Integral a => Int32le -> a
normalizeTextSize size = case int32leValue size of
  0x05000000 -> 8
  x -> if x < 0 then (-2 * fromIntegral x) else fromIntegral x

getTextDecoder :: Int32le -> ByteString.ByteString -> Text.Text
getTextDecoder size bytes =
  let
    decode = if size < Int32le 0
      then Encoding.decodeUtf16LE
      else Encoding.decodeLatin1
  in
    decode (ByteString.toStrict bytes)

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
