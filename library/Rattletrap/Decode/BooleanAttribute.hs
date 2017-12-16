module Rattletrap.Decode.BooleanAttribute
  ( decodeBooleanAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.BooleanAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeBooleanAttributeBits :: DecodeBits BooleanAttribute
decodeBooleanAttributeBits = BooleanAttribute <$> BinaryBits.getBool
