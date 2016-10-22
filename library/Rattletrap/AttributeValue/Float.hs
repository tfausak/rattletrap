module Rattletrap.AttributeValue.Float where

import Rattletrap.Float32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data FloatAttributeValue = FloatAttributeValue
  { floatAttributeValueFloat :: Float32
  } deriving (Eq, Ord, Show)

getFloatAttributeValue :: BinaryBit.BitGet FloatAttributeValue
getFloatAttributeValue = do
  float <- getFloat32Bits
  pure (FloatAttributeValue float)

putFloatAttributeValue :: FloatAttributeValue -> BinaryBit.BitPut ()
putFloatAttributeValue floatAttributeValue =
  putFloat32Bits (floatAttributeValueFloat floatAttributeValue)
