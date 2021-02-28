module Rattletrap.Type.Str where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import Rattletrap.Utility.Bytes
import qualified Rattletrap.Utility.Json as Json

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Bytes
import qualified Data.Char as Char
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Debug.Trace as Debug

newtype Str
  = Str Text.Text
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON Str where
  parseJSON = fmap fromText . Aeson.parseJSON

instance Aeson.ToJSON Str where
  toJSON = Aeson.toJSON . toText

schema :: Schema.Schema
schema = Schema.named "str" $ Aeson.object [Json.pair "type" "string"]

fromText :: Text.Text -> Str
fromText = Str

toText :: Str -> Text.Text
toText (Str x) = x

fromString :: String -> Str
fromString = fromText . Text.pack

toString :: Str -> String
toString = Text.unpack . toText

bytePut :: Str -> BytePut.BytePut
bytePut text =
  let
    size = getTextSize text
    encode = getTextEncoder size
  in I32.bytePut size <> (BytePut.byteString . encode . addNull $ toText text)

bitPut :: Str -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

getTextSize :: Str -> I32.I32
getTextSize text =
  let
    value = toText text
    scale = if Text.all Char.isLatin1 value then 1 else -1 :: Int.Int32
    rawSize = if Text.null value
      then 0
      else fromIntegral (Text.length value) + 1 :: Int.Int32
    size = if value == Text.pack "\x00\x00\x00None"
      then 0x05000000
      else scale * rawSize :: Int.Int32
  in I32.fromInt32 size

getTextEncoder :: I32.I32 -> Text.Text -> Bytes.ByteString
getTextEncoder size text =
  if I32.toInt32 size < 0 then Text.encodeUtf16LE text else encodeLatin1 text

addNull :: Text.Text -> Text.Text
addNull text = if Text.null text then text else Text.snoc text '\x00'

byteGet :: ByteGet.ByteGet Str
byteGet = do
  rawSize <- I32.byteGet
  bytes <- ByteGet.byteString (normalizeTextSize rawSize)
  pure (fromText (dropNull (getTextDecoder rawSize bytes)))

bitGet :: BitGet.BitGet Str
bitGet = do
  rawSize <- I32.bitGet
  bytes <- BitGet.byteString (normalizeTextSize rawSize)
  pure (fromText (dropNull (getTextDecoder rawSize (reverseBytes bytes))))

normalizeTextSize :: Integral a => I32.I32 -> a
normalizeTextSize size = case I32.toInt32 size of
  0x05000000 -> 8
  x -> if x < 0 then (-2 * fromIntegral x) else fromIntegral x

getTextDecoder :: I32.I32 -> Bytes.ByteString -> Text.Text
getTextDecoder size bytes =
  let
    decode = if I32.toInt32 size < 0
      then Text.decodeUtf16LEWith $ \message input -> do
        Debug.traceM $ "WARNING: " <> show (Text.DecodeError message input)
        Text.lenientDecode message input
      else Text.decodeLatin1
  in decode bytes

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
