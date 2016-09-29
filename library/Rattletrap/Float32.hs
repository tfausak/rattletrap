module Rattletrap.Float32 where

import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as IEEE754

newtype Float32 = Float32
  { float32Value :: Float
  } deriving (Eq, Ord, Show)

getFloat32 :: Binary.Get Float32
getFloat32 = do
  float32 <- IEEE754.getFloat32le
  pure (Float32 float32)

putFloat32 :: Float32 -> Binary.Put
putFloat32 (Float32 float32) = IEEE754.putFloat32le float32
