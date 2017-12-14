module Rattletrap.Encode.BooleanAttribute
  ( putBooleanAttribute
  ) where

import Rattletrap.Type.BooleanAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putBooleanAttribute :: BooleanAttribute -> BinaryBit.BitPut ()
putBooleanAttribute booleanAttribute =
  BinaryBit.putBool (booleanAttributeValue booleanAttribute)
