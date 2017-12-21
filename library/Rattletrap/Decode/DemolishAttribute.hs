module Rattletrap.Decode.DemolishAttribute
  ( decodeDemolishAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word32le
import Rattletrap.Type.DemolishAttribute

decodeDemolishAttributeBits :: DecodeBits DemolishAttribute
decodeDemolishAttributeBits =
  DemolishAttribute
    <$> getBool
    <*> decodeWord32leBits
    <*> getBool
    <*> decodeWord32leBits
    <*> decodeVectorBits
    <*> decodeVectorBits
