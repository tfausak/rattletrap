module Rattletrap.Encode.ByteAttribute
  ( putByteAttribute
  ) where

import Rattletrap.Type.ByteAttribute
import Rattletrap.Encode.Word8le

import qualified Data.Binary.Bits.Put as BinaryBit

putByteAttribute :: ByteAttribute -> BinaryBit.BitPut ()
putByteAttribute byteAttribute =
  putWord8Bits (byteAttributeValue byteAttribute)
