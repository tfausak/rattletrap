module Rattletrap.Decode.AppliedDamageAttribute
  ( decodeAppliedDamageAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word8le
import Rattletrap.Type.AppliedDamageAttribute

decodeAppliedDamageAttributeBits :: (Int, Int, Int) -> DecodeBits AppliedDamageAttribute
decodeAppliedDamageAttributeBits version =
  AppliedDamageAttribute
    <$> decodeWord8leBits
    <*> decodeVectorBits version
    <*> decodeInt32leBits
    <*> decodeInt32leBits
