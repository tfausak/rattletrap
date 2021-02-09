module Rattletrap.Encode.EnumAttribute
  ( putEnumAttribute
  ) where

import Rattletrap.Encode.Common
import Rattletrap.Type.EnumAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putEnumAttribute :: EnumAttribute -> BinaryBits.BitPut ()
putEnumAttribute enumAttribute =
  putBitsLE 11 (enumAttributeValue enumAttribute)
