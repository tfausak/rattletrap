module Rattletrap.Decode.DamageStateAttribute
  ( decodeDamageStateAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word8le
import Rattletrap.Type.DamageStateAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

decodeDamageStateAttributeBits :: DecodeBits DamageStateAttribute
decodeDamageStateAttributeBits =
  DamageStateAttribute
    <$> decodeWord8leBits
    <*> BinaryBit.getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits
    <*> BinaryBit.getBool
    <*> BinaryBit.getBool
