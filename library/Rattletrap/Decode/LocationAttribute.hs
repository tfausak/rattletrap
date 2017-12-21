module Rattletrap.Decode.LocationAttribute
  ( decodeLocationAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Vector
import Rattletrap.Type.LocationAttribute

decodeLocationAttributeBits :: DecodeBits LocationAttribute
decodeLocationAttributeBits = LocationAttribute <$> decodeVectorBits
