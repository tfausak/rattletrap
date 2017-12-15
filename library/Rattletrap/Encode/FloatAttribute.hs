module Rattletrap.Encode.FloatAttribute
  ( putFloatAttribute
  ) where

import Rattletrap.Encode.Float32le
import Rattletrap.Type.FloatAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putFloatAttribute :: FloatAttribute -> BinaryBit.BitPut ()
putFloatAttribute floatAttribute =
  putFloat32Bits (floatAttributeValue floatAttribute)
