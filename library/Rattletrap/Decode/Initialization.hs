module Rattletrap.Decode.Initialization
  ( decodeInitializationBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int8Vector
import Rattletrap.Decode.Vector
import Rattletrap.Type.Initialization

decodeInitializationBits :: Bool -> Bool -> DecodeBits Initialization
decodeInitializationBits hasLocation hasRotation =
  Initialization
    <$> decodeWhen hasLocation decodeVectorBits
    <*> decodeWhen hasRotation decodeInt8VectorBits
