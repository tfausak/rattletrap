module Rattletrap.Text where

import Rattletrap.Int32
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
  size <- getInt32
  bytes <- Binary.getLazyByteString (fromIntegral (int32Value size))
  let text = Encoding.decodeUtf8 (ByteString.toStrict bytes)
  pure (Text size text)

putText :: Text -> Binary.Put
putText text = do
  putInt32 (textSize text)
  Binary.putByteString (Encoding.encodeUtf8 (textValue text))

getTextBits :: BinaryBit.BitGet Text
getTextBits = do
  size <- getInt32Bits
  bytes <- BinaryBit.getLazyByteString (fromIntegral (int32Value size))
  let text = Encoding.decodeUtf8 (ByteString.toStrict (reverseBytes bytes))
  pure (Text size text)

putTextBits :: Text -> BinaryBit.BitPut ()
putTextBits text = do
  putInt32Bits (textSize text)
  BinaryBit.putByteString
    (ByteString.toStrict
       (reverseBytes
          (ByteString.fromStrict (Encoding.encodeUtf8 (textValue text)))))

stringToText :: String -> Text
stringToText string =
  let value = Text.snoc (Text.pack string) '\x00'
      size = Int32 (fromIntegral (Text.length value))
  in Text size value

textToString :: Text -> String
textToString text = Text.unpack (Text.dropWhileEnd (== '\x00') (textValue text))
