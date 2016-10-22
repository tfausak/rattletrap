module Rattletrap.AttributeValue.Enum where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

newtype EnumAttributeValue = EnumAttributeValue
  { enumAttributeValueValue :: Word.Word16
  } deriving (Eq, Ord, Show)

getEnumAttributeValue :: BinaryBit.BitGet EnumAttributeValue
getEnumAttributeValue = do
  value <- BinaryBit.getWord16be 11
  pure (EnumAttributeValue value)

putEnumAttributeValue :: EnumAttributeValue -> BinaryBit.BitPut ()
putEnumAttributeValue enumAttributeValue =
  BinaryBit.putWord16be 11 (enumAttributeValueValue enumAttributeValue)
