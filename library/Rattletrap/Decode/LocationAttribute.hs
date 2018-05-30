module Rattletrap.Decode.LocationAttribute
  ( decodeLocationAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Vector
import Rattletrap.Type.LocationAttribute

decodeLocationAttributeBits :: (Int, Int, Int) -> DecodeBits LocationAttribute
decodeLocationAttributeBits version = LocationAttribute <$> decodeVectorBits version
