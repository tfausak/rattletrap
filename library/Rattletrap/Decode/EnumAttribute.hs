module Rattletrap.Decode.EnumAttribute
  ( decodeEnumAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.EnumAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

decodeEnumAttributeBits :: DecodeBits EnumAttribute
decodeEnumAttributeBits = EnumAttribute <$> BinaryBit.getWord16be 11
