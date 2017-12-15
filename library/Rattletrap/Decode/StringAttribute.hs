module Rattletrap.Decode.StringAttribute
  ( getStringAttribute
  ) where

import Rattletrap.Decode.Str
import Rattletrap.Type.StringAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getStringAttribute :: BinaryBit.BitGet StringAttribute
getStringAttribute = do
  value <- getTextBits
  pure (StringAttribute value)
