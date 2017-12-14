module Rattletrap.Encode.Text
  ( putText
  , putTextBits
  ) where

import Rattletrap.Primitive.Int32
import Rattletrap.Type.Text
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

putText :: Text -> Binary.Put
putText text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32 size
  Binary.putLazyByteString (encode (addNull (textValue text)))

putTextBits :: Text -> BinaryBit.BitPut ()
putTextBits text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32Bits size
  BinaryBit.putByteString
    (ByteString.toStrict (reverseBytes (encode (addNull (textValue text)))))

getTextSize :: Text -> Int32
getTextSize text =
  let
    value = textValue text
    scale = if Text.all Char.isLatin1 value then 1 else -1
    rawSize =
      if Text.null value then 0 else fromIntegral (Text.length value) + 1
    size = if value == Text.pack "\x00\x00\x00None"
      then 0x05000000
      else scale * rawSize
  in
    Int32 size

getTextEncoder :: Int32 -> Text.Text -> ByteString.ByteString
getTextEncoder size text = if size < Int32 0
  then ByteString.fromStrict (Encoding.encodeUtf16LE text)
  else encodeLatin1 text

addNull :: Text.Text -> Text.Text
addNull text = if Text.null text then text else Text.snoc text '\x00'
