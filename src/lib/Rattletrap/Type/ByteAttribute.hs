{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ByteAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8le
  } deriving (Eq, Show)

$(deriveJson ''ByteAttribute)

putByteAttribute :: ByteAttribute -> BinaryBits.BitPut ()
putByteAttribute byteAttribute =
  putWord8Bits (byteAttributeValue byteAttribute)

decodeByteAttributeBits :: DecodeBits ByteAttribute
decodeByteAttributeBits = ByteAttribute <$> decodeWord8leBits
