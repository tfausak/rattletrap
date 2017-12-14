module Rattletrap.Decode.EnumAttribute
  ( getEnumAttribute
  ) where

import Rattletrap.Type.EnumAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getEnumAttribute :: BinaryBit.BitGet EnumAttribute
getEnumAttribute = do
  value <- BinaryBit.getWord16be 11
  pure (EnumAttribute value)
