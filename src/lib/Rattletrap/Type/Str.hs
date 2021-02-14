{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Str where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Text.Encoding.Error as Text
import qualified Debug.Trace as Debug
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

newtype Str
  = Str Text
  deriving (Eq, Ord, Show)

$(deriveJson ''Str)

fromText :: Text -> Str
fromText = Str

toText :: Str -> Text
toText (Str x) = x

fromString :: String -> Str
fromString = fromText . Text.pack

toString :: Str -> String
toString = Text.unpack . toText

bytePut :: Str -> BytePut
bytePut text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  Int32le.bytePut size
  Binary.putByteString (encode (addNull (toText text)))

bitPut :: Str -> BitPut ()
bitPut = bytePutToBitPut bytePut

getTextSize :: Str -> Int32le.Int32le
getTextSize text =
  let
    value = toText text
    scale = if Text.all Char.isLatin1 value then 1 else -1 :: Int32
    rawSize = if Text.null value
      then 0
      else fromIntegral (Text.length value) + 1 :: Int32
    size = if value == Text.pack "\x00\x00\x00None"
      then 0x05000000
      else scale * rawSize :: Int32
  in Int32le.fromInt32 size

getTextEncoder :: Int32le.Int32le -> Text.Text -> Bytes.ByteString
getTextEncoder size text =
  if Int32le.toInt32 size < 0 then Text.encodeUtf16LE text else encodeLatin1 text

addNull :: Text.Text -> Text.Text
addNull text = if Text.null text then text else Text.snoc text '\x00'

byteGet :: ByteGet Str
byteGet = do
  rawSize <- Int32le.byteGet
  bytes <- getByteString (normalizeTextSize rawSize)
  pure (fromText (dropNull (getTextDecoder rawSize bytes)))

bitGet :: BitGet Str
bitGet = do
  rawSize <- Int32le.bitGet
  bytes <- getByteStringBits (normalizeTextSize rawSize)
  pure (fromText (dropNull (getTextDecoder rawSize (reverseBytes bytes))))

normalizeTextSize :: Integral a => Int32le.Int32le -> a
normalizeTextSize size = case Int32le.toInt32 size of
  0x05000000 -> 8
  x -> if x < 0 then (-2 * fromIntegral x) else fromIntegral x

getTextDecoder :: Int32le.Int32le -> Bytes.ByteString -> Text.Text
getTextDecoder size bytes =
  let
    decode = if Int32le.toInt32 size < 0
      then Text.decodeUtf16LEWith $ \message input -> do
        Debug.traceM $ "WARNING: " <> show (Text.DecodeError message input)
        Text.lenientDecode message input
      else Text.decodeLatin1
  in decode bytes

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
