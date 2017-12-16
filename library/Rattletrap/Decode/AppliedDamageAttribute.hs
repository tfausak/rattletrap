module Rattletrap.Decode.AppliedDamageAttribute
  ( decodeAppliedDamageAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word8le
import Rattletrap.Type.AppliedDamageAttribute

decodeAppliedDamageAttributeBits :: DecodeBits AppliedDamageAttribute
decodeAppliedDamageAttributeBits =
  AppliedDamageAttribute
    <$> getWord8Bits
    <*> getVector
    <*> getInt32Bits
    <*> getInt32Bits
