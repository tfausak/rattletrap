module Rattletrap.AttributeValue.QWord where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype QWordAttributeValue = QWordAttributeValue
  { qWordAttributeValueValue :: Word64
  } deriving (Eq, Ord, Show)

getQWordAttributeValue :: BinaryBit.BitGet QWordAttributeValue
getQWordAttributeValue = do
  value <- getWord64Bits
  pure (QWordAttributeValue value)

putQWordAttributeValue :: QWordAttributeValue -> BinaryBit.BitPut ()
putQWordAttributeValue qWordAttributeValue =
  putWord64Bits (qWordAttributeValueValue qWordAttributeValue)
