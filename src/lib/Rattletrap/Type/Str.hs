{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Str where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Text.Encoding.Error as Text
import qualified Debug.Trace as Debug
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

newtype Str = Str
  { strValue :: Text
  } deriving (Eq, Ord, Show)

$(deriveJson ''Str)

toStr :: String -> Str
toStr string = Str (Text.pack string)

fromStr :: Str -> String
fromStr text = Text.unpack (strValue text)

putText :: Str -> Binary.Put
putText text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32 size
  Binary.putByteString (encode (addNull (strValue text)))

putTextBits :: Str -> BitPut ()
putTextBits text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32Bits size
  BinaryBits.putByteString (reverseBytes (encode (addNull (strValue text))))

getTextSize :: Str -> Int32le
getTextSize text =
  let
    value = strValue text
    scale = if Text.all Char.isLatin1 value then 1 else -1 :: Int32
    rawSize = if Text.null value
      then 0
      else fromIntegral (Text.length value) + 1 :: Int32
    size = if value == Text.pack "\x00\x00\x00None"
      then 0x05000000
      else scale * rawSize :: Int32
  in Int32le size

getTextEncoder :: Int32le -> Text.Text -> Bytes.ByteString
getTextEncoder size text =
  if int32leValue size < 0 then Text.encodeUtf16LE text else encodeLatin1 text

addNull :: Text.Text -> Text.Text
addNull text = if Text.null text then text else Text.snoc text '\x00'

decodeStr :: ByteGet Str
decodeStr = do
  rawSize <- decodeInt32le
  bytes <- getByteString (normalizeTextSize rawSize)
  pure (Str (dropNull (getTextDecoder rawSize bytes)))

decodeStrBits :: BitGet Str
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
    decode = if int32leValue size < 0
      then Text.decodeUtf16LEWith $ \message input -> do
        Debug.traceM $ "WARNING: " <> show (Text.DecodeError message input)
        Text.lenientDecode message input
      else Text.decodeLatin1
  in decode bytes

dropNull :: Text.Text -> Text.Text
dropNull = Text.dropWhileEnd (== '\x00')
