module Rattletrap.Attribute.Byte where

import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.Encode.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8
  } deriving (Eq, Ord, Show)

getByteAttribute :: BinaryBit.BitGet ByteAttribute
getByteAttribute = do
  value <- getWord8Bits
  pure (ByteAttribute value)

putByteAttribute :: ByteAttribute -> BinaryBit.BitPut ()
putByteAttribute byteAttribute =
  putWord8Bits (byteAttributeValue byteAttribute)
