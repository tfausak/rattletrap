module Rattletrap.Utility.Bytes where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Text as Text

encodeLatin1 :: Text.Text -> ByteString.ByteString
encodeLatin1 text = Latin1.pack (Text.unpack text)

padBytes :: Integral a => a -> ByteString.ByteString -> ByteString.ByteString
padBytes size bytes =
  bytes <> ByteString.replicate (fromIntegral size - ByteString.length bytes) 0x00
