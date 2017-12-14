module Rattletrap.Encode.LoadoutsAttribute
  ( putLoadoutsAttribute
  ) where

import Rattletrap.Type.LoadoutsAttribute
import Rattletrap.Encode.LoadoutAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putLoadoutsAttribute :: LoadoutsAttribute -> BinaryBit.BitPut ()
putLoadoutsAttribute loadoutsAttribute = do
  putLoadoutAttribute (loadoutsAttributeBlue loadoutsAttribute)
  putLoadoutAttribute (loadoutsAttributeOrange loadoutsAttribute)
