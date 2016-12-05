module Rattletrap.Attribute.Enum where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word.Word16
  } deriving (Eq, Ord, Show)

getEnumAttribute :: BinaryBit.BitGet EnumAttribute
getEnumAttribute = do
  value <- BinaryBit.getWord16be 11
  pure (EnumAttribute value)

putEnumAttribute :: EnumAttribute -> BinaryBit.BitPut ()
putEnumAttribute enumAttribute =
  BinaryBit.putWord16be 11 (enumAttributeValue enumAttribute)
