module Rattletrap.Decode.EnumAttribute
  ( decodeEnumAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.EnumAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeEnumAttributeBits :: DecodeBits EnumAttribute
decodeEnumAttributeBits = EnumAttribute <$> BinaryBits.getWord16be 11
