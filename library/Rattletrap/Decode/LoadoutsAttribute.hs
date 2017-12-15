module Rattletrap.Decode.LoadoutsAttribute
  ( getLoadoutsAttribute
  ) where

import Rattletrap.Decode.LoadoutAttribute
import Rattletrap.Type.LoadoutsAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getLoadoutsAttribute :: BinaryBit.BitGet LoadoutsAttribute
getLoadoutsAttribute = do
  blue <- getLoadoutAttribute
  orange <- getLoadoutAttribute
  pure (LoadoutsAttribute blue orange)
