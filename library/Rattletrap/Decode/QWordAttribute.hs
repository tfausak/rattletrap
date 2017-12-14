module Rattletrap.Decode.QWordAttribute
  ( getQWordAttribute
  ) where

import Rattletrap.Type.QWordAttribute
import Rattletrap.Decode.Word64

import qualified Data.Binary.Bits.Get as BinaryBit

getQWordAttribute :: BinaryBit.BitGet QWordAttribute
getQWordAttribute = do
  value <- getWord64Bits
  pure (QWordAttribute value)
