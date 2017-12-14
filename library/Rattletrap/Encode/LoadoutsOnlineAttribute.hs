module Rattletrap.Encode.LoadoutsOnlineAttribute
  ( putLoadoutsOnlineAttribute
  ) where

import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Encode.LoadoutOnlineAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putLoadoutsOnlineAttribute :: LoadoutsOnlineAttribute -> BinaryBit.BitPut ()
putLoadoutsOnlineAttribute loadoutsOnlineAttribute = do
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeBlue loadoutsOnlineAttribute)
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeOrange loadoutsOnlineAttribute)
  BinaryBit.putBool (loadoutsOnlineAttributeUnknown1 loadoutsOnlineAttribute)
  BinaryBit.putBool (loadoutsOnlineAttributeUnknown2 loadoutsOnlineAttribute)
