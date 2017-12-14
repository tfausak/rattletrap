module Rattletrap.Encode.FloatAttribute
  ( putFloatAttribute
  ) where

import Rattletrap.Type.FloatAttribute
import Rattletrap.Encode.Float32

import qualified Data.Binary.Bits.Put as BinaryBit

putFloatAttribute :: FloatAttribute -> BinaryBit.BitPut ()
putFloatAttribute floatAttribute =
  putFloat32Bits (floatAttributeValue floatAttribute)
