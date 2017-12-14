module Rattletrap.Encode.LocationAttribute
  ( putLocationAttribute
  ) where

import Rattletrap.Type.LocationAttribute
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putLocationAttribute :: LocationAttribute -> BinaryBit.BitPut ()
putLocationAttribute locationAttribute =
  putVector (locationAttributeValue locationAttribute)
