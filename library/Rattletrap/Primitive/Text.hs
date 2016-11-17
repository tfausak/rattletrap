module Rattletrap.Primitive.Text where

import Rattletrap.Primitive.Int32
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

newtype Text = Text
  { textValue :: Text.Text
  } deriving (Eq, Ord, Show)

getText :: Binary.Get Text
getText = do
  rawSize <- getInt32
  let decode = getTextDecoder rawSize
  let size = normalizeTextSize rawSize
  bytes <- Binary.getLazyByteString size
  let text = dropNull (decode bytes)
  pure (Text text)

putText :: Text -> Binary.Put
putText text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32 size
  Binary.putLazyByteString (encode (addNull (textValue text)))

getTextBits :: BinaryBit.BitGet Text
getTextBits = do
  rawSize <- getInt32Bits
  let decode = getTextDecoder rawSize
  let size = normalizeTextSize rawSize
  bytes <- BinaryBit.getLazyByteString size
  let text = dropNull (decode (reverseBytes bytes))
  pure (Text text)

putTextBits :: Text -> BinaryBit.BitPut ()
putTextBits text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32Bits size
  BinaryBit.putByteString
    (ByteString.toStrict (reverseBytes (encode (addNull (textValue text)))))

stringToText :: String -> Text
stringToText string = Text (Text.pack string)

textToString :: Text -> String
textToString text = Text.unpack (textValue text)

getTextSize :: Text -> Int32
getTextSize text =
  let value = textValue text
      scale =
        if Text.all Char.isLatin1 value
          then 1
          else -1
      rawSize =
        if Text.null value
          then 0
          else fromIntegral (Text.length value) + 1
      size =
        if value == Text.pack "\x00\x00\x00None"
          then 0x05000000
          else scale * rawSize
  in Int32 size

normalizeTextSize
  :: Integral a
  => Int32 -> a
normalizeTextSize size =
  case int32Value size of
    0x05000000 -> 8
    x ->
      if x < 0
        then (-2 * fromIntegral x)
        else fromIntegral x

getTextDecoder :: Int32 -> ByteString.ByteString -> Text.Text
getTextDecoder size bytes =
  let decode =
        if size < Int32 0
          then Encoding.decodeUtf16LE
          else Encoding.decodeLatin1
  in decode (ByteString.toStrict bytes)

getTextEncoder :: Int32 -> Text.Text -> ByteString.ByteString
getTextEncoder size text =
  if size < Int32 0
    then ByteString.fromStrict (Encoding.encodeUtf16LE text)
    else encodeLatin1 text

encodeLatin1 :: Text.Text -> ByteString.ByteString
encodeLatin1 text = ByteString.pack (Text.unpack text)

dropNull :: Text.Text -> Text.Text
dropNull text = Text.dropWhileEnd (== '\x00') text

addNull :: Text.Text -> Text.Text
addNull text =
  if Text.null text
    then text
    else Text.snoc text '\x00'
