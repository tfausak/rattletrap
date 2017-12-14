module Rattletrap.Decode.StringAttribute
  ( getStringAttribute
  ) where

import Rattletrap.Type.StringAttribute
import Rattletrap.Decode.Text

import qualified Data.Binary.Bits.Get as BinaryBit

getStringAttribute :: BinaryBit.BitGet StringAttribute
getStringAttribute = do
  value <- getTextBits
  pure (StringAttribute value)
