module Rattletrap.Decode.CustomDemolishAttribute
  ( decodeCustomDemolishAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.DemolishAttribute
import Rattletrap.Decode.Int32le
import Rattletrap.Type.CustomDemolishAttribute

decodeCustomDemolishAttributeBits
  :: (Int, Int, Int) -> DecodeBits CustomDemolishAttribute
decodeCustomDemolishAttributeBits version =
  CustomDemolishAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> decodeDemolishAttributeBits version
