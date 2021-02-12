module Rattletrap.Encode.ByteAttribute
  ( putByteAttribute
  ) where

import Rattletrap.Type.Word8le
import Rattletrap.Type.ByteAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putByteAttribute :: ByteAttribute -> BinaryBits.BitPut ()
putByteAttribute byteAttribute =
  putWord8Bits (byteAttributeValue byteAttribute)
