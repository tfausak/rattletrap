module Rattletrap.Encode.LoadoutsOnlineAttribute
  ( putLoadoutsOnlineAttribute
  ) where

import Rattletrap.Encode.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putLoadoutsOnlineAttribute :: LoadoutsOnlineAttribute -> BinaryBits.BitPut ()
putLoadoutsOnlineAttribute loadoutsOnlineAttribute = do
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeBlue loadoutsOnlineAttribute)
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeOrange loadoutsOnlineAttribute)
  BinaryBits.putBool (loadoutsOnlineAttributeUnknown1 loadoutsOnlineAttribute)
  BinaryBits.putBool (loadoutsOnlineAttributeUnknown2 loadoutsOnlineAttribute)
