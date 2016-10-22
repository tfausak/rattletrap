module Rattletrap.AttributeValue.Boolean where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype BooleanAttributeValue = BooleanAttributeValue
  { booleanAttributeValueValue :: Bool
  } deriving (Eq, Ord, Show)

getBooleanAttributeValue :: BinaryBit.BitGet BooleanAttributeValue
getBooleanAttributeValue = do
  value <- BinaryBit.getBool
  pure (BooleanAttributeValue value)

putBooleanAttributeValue :: BooleanAttributeValue -> BinaryBit.BitPut ()
putBooleanAttributeValue booleanAttributeValue =
  BinaryBit.putBool (booleanAttributeValueValue booleanAttributeValue)
