module Rattletrap.Decode.FlaggedIntAttribute
  ( getFlaggedIntAttribute
  ) where

import Rattletrap.Decode.Int32le
import Rattletrap.Type.FlaggedIntAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getFlaggedIntAttribute :: BinaryBit.BitGet FlaggedIntAttribute
getFlaggedIntAttribute = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttribute flag int)
