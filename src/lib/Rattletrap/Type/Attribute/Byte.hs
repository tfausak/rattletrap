{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Byte where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8le
  } deriving (Eq, Show)

$(deriveJson ''ByteAttribute)

putByteAttribute :: ByteAttribute -> BitPut ()
putByteAttribute byteAttribute =
  putWord8Bits (byteAttributeValue byteAttribute)

decodeByteAttributeBits :: BitGet ByteAttribute
decodeByteAttributeBits = ByteAttribute <$> decodeWord8leBits
