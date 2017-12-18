module Rattletrap.Decode.DemolishAttribute
  ( decodeDemolishAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word32le
import Rattletrap.Type.DemolishAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeDemolishAttributeBits :: DecodeBits DemolishAttribute
decodeDemolishAttributeBits =
  DemolishAttribute
    <$> BinaryBits.getBool
    <*> decodeWord32leBits
    <*> BinaryBits.getBool
    <*> decodeWord32leBits
    <*> decodeVectorBits
    <*> decodeVectorBits
