module Rattletrap.Attribute.Location where

import Rattletrap.Type.Vector
import Rattletrap.Decode.Vector
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype LocationAttribute = LocationAttribute
  { locationAttributeValue :: Vector
  } deriving (Eq, Ord, Show)

getLocationAttribute :: BinaryBit.BitGet LocationAttribute
getLocationAttribute = do
  value <- getVector
  pure (LocationAttribute value)

putLocationAttribute :: LocationAttribute -> BinaryBit.BitPut ()
putLocationAttribute locationAttribute =
  putVector (locationAttributeValue locationAttribute)
