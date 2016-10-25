module Rattletrap.AttributeValue.LoadoutsOnline where

import Rattletrap.AttributeValue.LoadoutOnline

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data LoadoutsOnlineAttributeValue = LoadoutsOnlineAttributeValue
  { loadoutsOnlineAttributeValueBlue :: LoadoutOnlineAttributeValue
  , loadoutsOnlineAttributeValueOrange :: LoadoutOnlineAttributeValue
  , loadoutsOnlineAttributeValueUnknown1 :: Bool
  , loadoutsOnlineAttributeValueUnknown2 :: Bool
  } deriving (Eq, Ord, Show)

getLoadoutsOnlineAttributeValue :: BinaryBit.BitGet LoadoutsOnlineAttributeValue
getLoadoutsOnlineAttributeValue = do
  blueLoadout <- getLoadoutOnlineAttributeValue
  orangeLoadout <- getLoadoutOnlineAttributeValue
  unknown1 <- BinaryBit.getBool
  unknown2 <- BinaryBit.getBool
  pure
    (LoadoutsOnlineAttributeValue blueLoadout orangeLoadout unknown1 unknown2)

putLoadoutsOnlineAttributeValue :: LoadoutsOnlineAttributeValue
                                -> BinaryBit.BitPut ()
putLoadoutsOnlineAttributeValue loadoutsOnlineAttributeValue = do
  putLoadoutOnlineAttributeValue
    (loadoutsOnlineAttributeValueBlue loadoutsOnlineAttributeValue)
  putLoadoutOnlineAttributeValue
    (loadoutsOnlineAttributeValueOrange loadoutsOnlineAttributeValue)
  BinaryBit.putBool
    (loadoutsOnlineAttributeValueUnknown1 loadoutsOnlineAttributeValue)
  BinaryBit.putBool
    (loadoutsOnlineAttributeValueUnknown2 loadoutsOnlineAttributeValue)
