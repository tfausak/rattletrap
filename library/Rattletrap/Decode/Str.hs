module Rattletrap.Decode.Str
  ( decodeStr
  , decodeStrBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Str
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

decodeStr :: Decode Str
decodeStr = do
  rawSize <- decodeInt32le
  bytes <- Binary.getLazyByteString (normalizeTextSize rawSize)
  pure (Str (dropNull (getTextDecoder rawSize bytes)))

decodeStrBits :: DecodeBits Str
decodeStrBits = do
  rawSize <- decodeInt32leBits
  bytes <- BinaryBits.getLazyByteString (normalizeTextSize rawSize)
  pure (Str (dropNull (getTextDecoder rawSize (reverseBytes bytes))))

normalizeTextSize :: Integral a => Int32le -> a
normalizeTextSize size = case int32leValue size of
  0x05000000 -> 8
  x -> if x < 0 then (-2 * fromIntegral x) else fromIntegral x

getTextDecoder :: Int32le -> LazyBytes.ByteString -> Text.Text
getTextDecoder size bytes =
  let
    decode = if size < Int32le 0
      then Text.decodeUtf16LE
      else Text.decodeLatin1
  in
    decode (LazyBytes.toStrict bytes)

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
