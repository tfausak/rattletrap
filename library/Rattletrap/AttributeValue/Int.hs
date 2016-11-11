module Rattletrap.AttributeValue.Int where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype IntAttributeValue = IntAttributeValue
  { intAttributeValueValue :: Int32
  } deriving (Eq, Ord, Show)

getIntAttributeValue :: BinaryBit.BitGet IntAttributeValue
getIntAttributeValue = do
  value <- getInt32Bits
  pure (IntAttributeValue value)

putIntAttributeValue :: IntAttributeValue -> BinaryBit.BitPut ()
putIntAttributeValue intAttributeValue =
  putInt32Bits (intAttributeValueValue intAttributeValue)
