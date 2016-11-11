module Rattletrap.Attribute.Int where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32
  } deriving (Eq, Ord, Show)

getIntAttribute :: BinaryBit.BitGet IntAttribute
getIntAttribute = do
  value <- getInt32Bits
  pure (IntAttribute value)

putIntAttribute :: IntAttribute -> BinaryBit.BitPut ()
putIntAttribute intAttribute =
  putInt32Bits (intAttributeValue intAttribute)
