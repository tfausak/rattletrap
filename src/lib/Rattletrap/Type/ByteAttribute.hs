{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ByteAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8le
  } deriving (Eq, Ord, Show)

$(deriveJson ''ByteAttribute)

putByteAttribute :: ByteAttribute -> BinaryBits.BitPut ()
putByteAttribute byteAttribute =
  putWord8Bits (byteAttributeValue byteAttribute)
