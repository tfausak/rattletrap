module Rattletrap.AttributeValue.Loadouts where

import Rattletrap.AttributeValue.Loadout

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data LoadoutsAttributeValue = LoadoutsAttributeValue
  { loadoutsAttributeValueBlue :: LoadoutAttributeValue
  , loadoutsAttributeValueOrange :: LoadoutAttributeValue
  } deriving (Eq, Ord, Show)

getLoadoutsAttributeValue :: BinaryBit.BitGet LoadoutsAttributeValue
getLoadoutsAttributeValue = do
  blue <- getLoadoutAttributeValue
  orange <- getLoadoutAttributeValue
  pure (LoadoutsAttributeValue blue orange)

putLoadoutsAttributeValue :: LoadoutsAttributeValue -> BinaryBit.BitPut ()
putLoadoutsAttributeValue loadoutsAttributeValue = do
  putLoadoutAttributeValue (loadoutsAttributeValueBlue loadoutsAttributeValue)
  putLoadoutAttributeValue (loadoutsAttributeValueOrange loadoutsAttributeValue)
