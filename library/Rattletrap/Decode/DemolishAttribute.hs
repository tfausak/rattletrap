module Rattletrap.Decode.DemolishAttribute
  ( decodeDemolishAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word32le
import Rattletrap.Type.DemolishAttribute

decodeDemolishAttributeBits :: (Int, Int, Int) -> DecodeBits DemolishAttribute
decodeDemolishAttributeBits version =
  DemolishAttribute
    <$> getBool
    <*> decodeWord32leBits
    <*> getBool
    <*> decodeWord32leBits
    <*> decodeVectorBits version
    <*> decodeVectorBits version
