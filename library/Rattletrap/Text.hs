module Rattletrap.Text where

import Rattletrap.Int32

import qualified Data.Binary as Binary
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
  let value = Encoding.decodeUtf8 (ByteString.toStrict bytes)
  pure Text {textSize = size, textValue = value}

putText :: Text -> Binary.Put
putText text = do
  putInt32 (textSize text)
  let bytes = Encoding.encodeUtf8 (textValue text)
  Binary.putByteString bytes

stringToText :: String -> Text
stringToText string =
  let value = Text.snoc (Text.pack string) '\x00'
      size = Int32 (fromIntegral (Text.length value))
  in Text {textSize = size, textValue = value}

textToString :: Text -> String
textToString text = Text.unpack (Text.dropWhileEnd (== '\x00') (textValue text))
