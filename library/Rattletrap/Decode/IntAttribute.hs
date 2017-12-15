module Rattletrap.Decode.IntAttribute
  ( getIntAttribute
  ) where

import Rattletrap.Decode.Int32le
import Rattletrap.Type.IntAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getIntAttribute :: BinaryBit.BitGet IntAttribute
getIntAttribute = do
  value <- getInt32Bits
  pure (IntAttribute value)
