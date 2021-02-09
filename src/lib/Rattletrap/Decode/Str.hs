module Rattletrap.Decode.Str
  ( decodeStr
  , decodeStrBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Str
import Rattletrap.Utility.Bytes

import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Debug.Trace as Debug

decodeStr :: Decode Str
decodeStr = do
  rawSize <- decodeInt32le
  bytes <- getByteString (normalizeTextSize rawSize)
  pure (Str (dropNull (getTextDecoder rawSize bytes)))

decodeStrBits :: DecodeBits Str
decodeStrBits = do
  rawSize <- decodeInt32leBits
  bytes <- getByteStringBits (normalizeTextSize rawSize)
  pure (Str (dropNull (getTextDecoder rawSize (reverseBytes bytes))))

normalizeTextSize :: Integral a => Int32le -> a
normalizeTextSize size = case int32leValue size of
  0x05000000 -> 8
  x -> if x < 0 then (-2 * fromIntegral x) else fromIntegral x

getTextDecoder :: Int32le -> Bytes.ByteString -> Text.Text
getTextDecoder size bytes =
  let
    decode = if size < Int32le 0
      then Text.decodeUtf16LEWith $ \message input -> do
        Debug.traceM $ "WARNING: " <> show (Text.DecodeError message input)
        Text.lenientDecode message input
      else Text.decodeLatin1
  in decode bytes

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
