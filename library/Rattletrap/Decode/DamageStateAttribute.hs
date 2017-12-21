module Rattletrap.Decode.DamageStateAttribute
  ( decodeDamageStateAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word8le
import Rattletrap.Type.DamageStateAttribute

decodeDamageStateAttributeBits :: DecodeBits DamageStateAttribute
decodeDamageStateAttributeBits =
  DamageStateAttribute
    <$> decodeWord8leBits
    <*> getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits
    <*> getBool
    <*> getBool
