module Rattletrap.Decode.BooleanAttribute
  ( getBooleanAttribute
  ) where

import Rattletrap.Type.BooleanAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getBooleanAttribute :: BinaryBit.BitGet BooleanAttribute
getBooleanAttribute = do
  value <- BinaryBit.getBool
  pure (BooleanAttribute value)
