module Rattletrap.Encode.EnumAttribute
  ( putEnumAttribute
  ) where

import Rattletrap.Type.EnumAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putEnumAttribute :: EnumAttribute -> BinaryBit.BitPut ()
putEnumAttribute enumAttribute =
  BinaryBit.putWord16be 11 (enumAttributeValue enumAttribute)
