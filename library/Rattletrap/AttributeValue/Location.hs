module Rattletrap.AttributeValue.Location where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype LocationAttributeValue = LocationAttributeValue
  { locationAttributeValueValue :: Vector
  } deriving (Eq, Ord, Show)

getLocationAttributeValue :: BinaryBit.BitGet LocationAttributeValue
getLocationAttributeValue = do
  value <- getVector
  pure (LocationAttributeValue value)

putLocationAttributeValue :: LocationAttributeValue -> BinaryBit.BitPut ()
putLocationAttributeValue locationAttributeValue =
  putVector (locationAttributeValueValue locationAttributeValue)
