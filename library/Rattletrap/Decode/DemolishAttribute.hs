module Rattletrap.Decode.DemolishAttribute
  ( decodeDemolishAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word32le
import Rattletrap.Type.DemolishAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

decodeDemolishAttributeBits :: DecodeBits DemolishAttribute
decodeDemolishAttributeBits =
  DemolishAttribute
    <$> BinaryBit.getBool
    <*> getWord32Bits
    <*> BinaryBit.getBool
    <*> getWord32Bits
    <*> getVector
    <*> getVector
