module Rattletrap.Encode.Str
  ( putText
  , putTextBits
  ) where

import Rattletrap.Encode.Int32le
import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Str
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

putText :: Str -> Binary.Put
putText text = do
  let size = getTextSize text
  let encode = getTextEncoder size
  putInt32 size
  Binary.putByteString (encode (addNull (strValue text)))

putTextBits :: Str -> BinaryBits.BitPut ()
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
  if size < Int32le 0 then Text.encodeUtf16LE text else encodeLatin1 text

addNull :: Text.Text -> Text.Text
addNull text = if Text.null text then text else Text.snoc text '\x00'
