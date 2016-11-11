module Rattletrap.Attribute.LoadoutsOnline where

import Rattletrap.Attribute.LoadoutOnline

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data LoadoutsOnlineAttribute = LoadoutsOnlineAttribute
  { loadoutsOnlineAttributeBlue :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeOrange :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeUnknown1 :: Bool
  , loadoutsOnlineAttributeUnknown2 :: Bool
  } deriving (Eq, Ord, Show)

getLoadoutsOnlineAttribute :: BinaryBit.BitGet LoadoutsOnlineAttribute
getLoadoutsOnlineAttribute = do
  blueLoadout <- getLoadoutOnlineAttribute
  orangeLoadout <- getLoadoutOnlineAttribute
  unknown1 <- BinaryBit.getBool
  unknown2 <- BinaryBit.getBool
  pure
    (LoadoutsOnlineAttribute blueLoadout orangeLoadout unknown1 unknown2)

putLoadoutsOnlineAttribute :: LoadoutsOnlineAttribute
                                -> BinaryBit.BitPut ()
putLoadoutsOnlineAttribute loadoutsOnlineAttribute = do
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeBlue loadoutsOnlineAttribute)
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeOrange loadoutsOnlineAttribute)
  BinaryBit.putBool
    (loadoutsOnlineAttributeUnknown1 loadoutsOnlineAttribute)
  BinaryBit.putBool
    (loadoutsOnlineAttributeUnknown2 loadoutsOnlineAttribute)
