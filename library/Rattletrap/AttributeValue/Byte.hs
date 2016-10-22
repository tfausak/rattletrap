module Rattletrap.AttributeValue.Byte where

import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ByteAttributeValue = ByteAttributeValue
  { byteAttributeValueWord :: Word8
  } deriving (Eq, Ord, Show)

getByteAttributeValue :: BinaryBit.BitGet ByteAttributeValue
getByteAttributeValue = do
  word <- getWord8Bits
  pure (ByteAttributeValue word)

putByteAttributeValue :: ByteAttributeValue -> BinaryBit.BitPut ()
putByteAttributeValue byteAttributeValue =
  putWord8Bits (byteAttributeValueWord byteAttributeValue)
