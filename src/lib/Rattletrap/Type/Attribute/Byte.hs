{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Byte where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8le.Word8le
  } deriving (Eq, Show)

$(deriveJson ''ByteAttribute)

putByteAttribute :: ByteAttribute -> BitPut ()
putByteAttribute byteAttribute =
  Word8le.bitPut (byteAttributeValue byteAttribute)

decodeByteAttributeBits :: BitGet ByteAttribute
decodeByteAttributeBits = ByteAttribute <$> Word8le.bitGet
