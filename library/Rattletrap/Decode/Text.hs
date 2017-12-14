module Rattletrap.Decode.Text
  ( getText
  , getTextBits
  ) where

import Rattletrap.Primitive.Int32
import Rattletrap.Type.Text
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

getText :: Binary.Get Text
getText = do
  rawSize <- getInt32
  let decode = getTextDecoder rawSize
  let size = normalizeTextSize rawSize
  bytes <- Binary.getLazyByteString size
  let text = dropNull (decode bytes)
  pure (Text text)

getTextBits :: BinaryBit.BitGet Text
getTextBits = do
  rawSize <- getInt32Bits
  let decode = getTextDecoder rawSize
  let size = normalizeTextSize rawSize
  bytes <- BinaryBit.getLazyByteString size
  let text = dropNull (decode (reverseBytes bytes))
  pure (Text text)

normalizeTextSize :: Integral a => Int32 -> a
normalizeTextSize size = case int32Value size of
  0x05000000 -> 8
  x -> if x < 0 then (-2 * fromIntegral x) else fromIntegral x

getTextDecoder :: Int32 -> ByteString.ByteString -> Text.Text
getTextDecoder size bytes =
  let
    decode =
      if size < Int32 0 then Encoding.decodeUtf16LE else Encoding.decodeLatin1
  in
    decode (ByteString.toStrict bytes)

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
