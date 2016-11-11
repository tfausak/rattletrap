module Rattletrap.AttributeValue.Byte where

import Rattletrap.Primitive.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype ByteAttributeValue = ByteAttributeValue
  { byteAttributeValueValue :: Word8
  } deriving (Eq, Ord, Show)

getByteAttributeValue :: BinaryBit.BitGet ByteAttributeValue
getByteAttributeValue = do
  value <- getWord8Bits
  pure (ByteAttributeValue value)

putByteAttributeValue :: ByteAttributeValue -> BinaryBit.BitPut ()
putByteAttributeValue byteAttributeValue =
  putWord8Bits (byteAttributeValueValue byteAttributeValue)
