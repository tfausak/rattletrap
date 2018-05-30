module Rattletrap.Decode.Initialization
  ( decodeInitializationBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int8Vector
import Rattletrap.Decode.Vector
import Rattletrap.Type.Initialization

decodeInitializationBits :: (Int, Int, Int) -> Bool -> Bool -> DecodeBits Initialization
decodeInitializationBits version hasLocation hasRotation =
  Initialization
    <$> decodeWhen hasLocation (decodeVectorBits version)
    <*> decodeWhen hasRotation decodeInt8VectorBits
