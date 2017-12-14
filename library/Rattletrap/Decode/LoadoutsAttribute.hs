module Rattletrap.Decode.LoadoutsAttribute
  ( getLoadoutsAttribute
  ) where

import Rattletrap.Type.LoadoutsAttribute
import Rattletrap.Decode.LoadoutAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getLoadoutsAttribute :: BinaryBit.BitGet LoadoutsAttribute
getLoadoutsAttribute = do
  blue <- getLoadoutAttribute
  orange <- getLoadoutAttribute
  pure (LoadoutsAttribute blue orange)
