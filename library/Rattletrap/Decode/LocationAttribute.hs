module Rattletrap.Decode.LocationAttribute
  ( getLocationAttribute
  ) where

import Rattletrap.Decode.Vector
import Rattletrap.Type.LocationAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getLocationAttribute :: BinaryBit.BitGet LocationAttribute
getLocationAttribute = do
  value <- getVector
  pure (LocationAttribute value)
