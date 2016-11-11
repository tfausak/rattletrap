module Rattletrap.AttributeValue.Float where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype FloatAttributeValue = FloatAttributeValue
  { floatAttributeValueValue :: Float32
  } deriving (Eq, Ord, Show)

getFloatAttributeValue :: BinaryBit.BitGet FloatAttributeValue
getFloatAttributeValue = do
  value <- getFloat32Bits
  pure (FloatAttributeValue value)

putFloatAttributeValue :: FloatAttributeValue -> BinaryBit.BitPut ()
putFloatAttributeValue floatAttributeValue =
  putFloat32Bits (floatAttributeValueValue floatAttributeValue)
