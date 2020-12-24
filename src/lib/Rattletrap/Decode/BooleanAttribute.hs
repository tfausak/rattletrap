module Rattletrap.Decode.BooleanAttribute
  ( decodeBooleanAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.BooleanAttribute

decodeBooleanAttributeBits :: DecodeBits BooleanAttribute
decodeBooleanAttributeBits = BooleanAttribute <$> getBool
