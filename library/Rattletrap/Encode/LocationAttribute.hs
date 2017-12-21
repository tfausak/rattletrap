module Rattletrap.Encode.LocationAttribute
  ( putLocationAttribute
  ) where

import Rattletrap.Encode.Vector
import Rattletrap.Type.LocationAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putLocationAttribute :: LocationAttribute -> BinaryBits.BitPut ()
putLocationAttribute locationAttribute =
  putVector (locationAttributeValue locationAttribute)
