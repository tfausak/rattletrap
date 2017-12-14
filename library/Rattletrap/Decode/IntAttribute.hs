module Rattletrap.Decode.IntAttribute
  ( getIntAttribute
  ) where

import Rattletrap.Type.IntAttribute
import Rattletrap.Decode.Int32

import qualified Data.Binary.Bits.Get as BinaryBit

getIntAttribute :: BinaryBit.BitGet IntAttribute
getIntAttribute = do
  value <- getInt32Bits
  pure (IntAttribute value)
