module Rattletrap.Decode.FloatAttribute
  ( getFloatAttribute
  ) where

import Rattletrap.Type.FloatAttribute
import Rattletrap.Decode.Float32le

import qualified Data.Binary.Bits.Get as BinaryBit

getFloatAttribute :: BinaryBit.BitGet FloatAttribute
getFloatAttribute = do
  value <- getFloat32Bits
  pure (FloatAttribute value)
