module Rattletrap.Encode.LoadoutsAttribute
  ( putLoadoutsAttribute
  )
where

import Rattletrap.Encode.LoadoutAttribute
import Rattletrap.Type.LoadoutsAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putLoadoutsAttribute :: LoadoutsAttribute -> BinaryBits.BitPut ()
putLoadoutsAttribute loadoutsAttribute = do
  putLoadoutAttribute (loadoutsAttributeBlue loadoutsAttribute)
  putLoadoutAttribute (loadoutsAttributeOrange loadoutsAttribute)
