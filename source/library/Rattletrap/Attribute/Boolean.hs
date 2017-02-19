module Rattletrap.Attribute.Boolean where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Show)

getBooleanAttribute :: BinaryBit.BitGet BooleanAttribute
getBooleanAttribute = do
  value <- BinaryBit.getBool
  pure (BooleanAttribute value)

putBooleanAttribute :: BooleanAttribute -> BinaryBit.BitPut ()
putBooleanAttribute booleanAttribute =
  BinaryBit.putBool (booleanAttributeValue booleanAttribute)
