module Rattletrap.Decode.FlaggedByteAttribute
  ( decodeFlaggedByteAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word8le
import Rattletrap.Type.FlaggedByteAttribute


decodeFlaggedByteAttributeBits :: DecodeBits FlaggedByteAttribute
decodeFlaggedByteAttributeBits = FlaggedByteAttribute <$> getBool <*> decodeWord8leBits
