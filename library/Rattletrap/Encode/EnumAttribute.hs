module Rattletrap.Encode.EnumAttribute
  ( putEnumAttribute
  )
where

import Rattletrap.Type.EnumAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putEnumAttribute :: EnumAttribute -> BinaryBits.BitPut ()
putEnumAttribute enumAttribute =
  BinaryBits.putWord16be 11 (enumAttributeValue enumAttribute)
