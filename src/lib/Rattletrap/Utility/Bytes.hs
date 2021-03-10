module Rattletrap.Utility.Bytes
  ( encodeLatin1
  , padBytes
  ) where

import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as Bytes8
import qualified Data.Text as Text

encodeLatin1 :: Text.Text -> Bytes.ByteString
encodeLatin1 text = Bytes8.pack (Text.unpack text)

padBytes :: Integral a => a -> Bytes.ByteString -> Bytes.ByteString
padBytes size bytes =
  bytes <> Bytes.replicate (fromIntegral size - Bytes.length bytes) 0x00
