module Rattletrap.Decode.EnumAttribute
  ( decodeEnumAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.EnumAttribute

decodeEnumAttributeBits :: DecodeBits EnumAttribute
decodeEnumAttributeBits = EnumAttribute <$> getBitsLE 11
