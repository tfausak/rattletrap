module Rattletrap.AttributeValue.Enum where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data EnumAttributeValue = EnumAttributeValue
  { enumAttributeValueWord :: Word.Word16
  } deriving (Eq, Ord, Show)

getEnumAttributeValue :: BinaryBit.BitGet EnumAttributeValue
getEnumAttributeValue = do
  word <- BinaryBit.getWord16be 11
  pure (EnumAttributeValue word)

putEnumAttributeValue :: EnumAttributeValue -> BinaryBit.BitPut ()
putEnumAttributeValue enumAttributeValue =
  BinaryBit.putWord16be 11 (enumAttributeValueWord enumAttributeValue)
