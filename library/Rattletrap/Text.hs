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

newtype Text = Text
  { textValue :: Text.Text
  } deriving (Eq, Ord, Show)

getText :: Binary.Get Text
getText = do
  size <- getInt32
  bytes <- Binary.getLazyByteString (fromIntegral (int32Value size))
  let text = Encoding.decodeUtf8 (ByteString.toStrict bytes)
  pure (Text text)

putText :: Text -> Binary.Put
putText (Text text) = do
  putInt32 (Int32 (fromIntegral (Text.length text)))
  Binary.putByteString (Encoding.encodeUtf8 text)

getTextBits :: BinaryBit.BitGet Text
getTextBits = do
  size <- getInt32Bits
  bytes <- BinaryBit.getLazyByteString (fromIntegral (int32Value size))
  let text = Encoding.decodeUtf8 (ByteString.toStrict (reverseBytes bytes))
  pure (Text text)

putTextBits :: Text -> BinaryBit.BitPut ()
putTextBits (Text text) = do
  putInt32Bits (Int32 (fromIntegral (Text.length text)))
  BinaryBit.putByteString
    (ByteString.toStrict
       (reverseBytes (ByteString.fromStrict (Encoding.encodeUtf8 text))))

stringToText :: String -> Text
stringToText string = Text (Text.snoc (Text.pack string) '\x00')

textToString :: Text -> String
textToString (Text text) = Text.unpack (Text.dropWhileEnd (== '\x00') text)
