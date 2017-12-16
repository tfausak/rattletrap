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
    <$> getWord8Bits
    <*> BinaryBit.getBool
    <*> getInt32Bits
    <*> getVector
    <*> BinaryBit.getBool
    <*> BinaryBit.getBool
