module Rattletrap.Decode.StringAttribute
  ( decodeStringAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Type.StringAttribute

decodeStringAttributeBits :: DecodeBits StringAttribute
decodeStringAttributeBits = StringAttribute <$> decodeStrBits
