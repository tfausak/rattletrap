module Rattletrap.Text where

import Rattletrap.Primitive.Int32
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

data Text = Text
  { textSize :: Int32
  , textValue :: Text.Text
  } deriving (Eq, Ord, Show)

getText :: Binary.Get Text
getText = do
  rawSize <- getInt32
  let decode = getTextDecoder rawSize
  let size = getTextSize rawSize
  bytes <- Binary.getLazyByteString size
  let text = decode bytes
  pure (Text rawSize text)

putText :: Text -> Binary.Put
putText text = do
  let size = textSize text
  let encode = getTextEncoder size
  putInt32 size
  Binary.putLazyByteString (encode (textValue text))

getTextBits :: BinaryBit.BitGet Text
getTextBits = do
  rawSize <- getInt32Bits
  let decode = getTextDecoder rawSize
  let size = getTextSize rawSize
  bytes <- BinaryBit.getLazyByteString size
  let text = decode (reverseBytes bytes)
  pure (Text rawSize text)

putTextBits :: Text -> BinaryBit.BitPut ()
putTextBits text = do
  let size = textSize text
  let encode = getTextEncoder size
  putInt32Bits size
  BinaryBit.putByteString
    (ByteString.toStrict (reverseBytes (encode (textValue text))))

stringToText :: String -> Text
stringToText string =
  let value = Text.snoc (Text.pack string) '\x00'
      size = Int32 (fromIntegral (Text.length value))
  in Text size value

textToString :: Text -> String
textToString text = Text.unpack (Text.dropWhileEnd (== '\x00') (textValue text))

getTextSize
  :: Integral a
  => Int32 -> a
getTextSize size =
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
