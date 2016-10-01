module Rattletrap.Text where

import Rattletrap.Int32

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy.Char8 as ByteString

data Text = Text
  { textSize :: Int32
  , textValue :: ByteString.ByteString
  } deriving (Eq, Ord, Show)

getText :: Binary.Get Text
getText = do
  size <- getInt32
  value <- Binary.getLazyByteString (fromIntegral (int32Value size))
  pure Text {textSize = size, textValue = value}

putText :: Text -> Binary.Put
putText text = do
  putInt32 (textSize text)
  Binary.putLazyByteString (textValue text)

stringToText :: String -> Text
stringToText string =
  let value = ByteString.snoc (ByteString.pack string) '\x00'
      size = Int32 (fromIntegral (ByteString.length value))
  in Text {textSize = size, textValue = value}
