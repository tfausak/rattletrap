module Rattletrap.Attribute.Loadouts where

import Rattletrap.Attribute.Loadout

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data LoadoutsAttribute = LoadoutsAttribute
  { loadoutsAttributeBlue :: LoadoutAttribute
  , loadoutsAttributeOrange :: LoadoutAttribute
  } deriving (Eq, Ord, Show)

getLoadoutsAttribute :: BinaryBit.BitGet LoadoutsAttribute
getLoadoutsAttribute = do
  blue <- getLoadoutAttribute
  orange <- getLoadoutAttribute
  pure (LoadoutsAttribute blue orange)

putLoadoutsAttribute :: LoadoutsAttribute -> BinaryBit.BitPut ()
putLoadoutsAttribute loadoutsAttribute = do
  putLoadoutAttribute (loadoutsAttributeBlue loadoutsAttribute)
  putLoadoutAttribute (loadoutsAttributeOrange loadoutsAttribute)
