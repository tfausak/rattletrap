module Rattletrap.Decode.ByteAttribute
  ( decodeByteAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word8le
import Rattletrap.Type.ByteAttribute

decodeByteAttributeBits :: DecodeBits ByteAttribute
decodeByteAttributeBits = ByteAttribute <$> getWord8Bits
