module Rattletrap.Decode.EnumAttribute
  ( decodeEnumAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.EnumAttribute

decodeEnumAttributeBits :: DecodeBits EnumAttribute
decodeEnumAttributeBits = EnumAttribute <$> getWord16be 11
