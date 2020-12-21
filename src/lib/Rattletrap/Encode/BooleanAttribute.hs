module Rattletrap.Encode.BooleanAttribute
  ( putBooleanAttribute
  )
where

import Rattletrap.Type.BooleanAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putBooleanAttribute :: BooleanAttribute -> BinaryBits.BitPut ()
putBooleanAttribute booleanAttribute =
  BinaryBits.putBool (booleanAttributeValue booleanAttribute)
