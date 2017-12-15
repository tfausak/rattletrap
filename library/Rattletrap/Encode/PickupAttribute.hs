module Rattletrap.Encode.PickupAttribute
  ( putPickupAttribute
  ) where

import Rattletrap.Encode.Word32le
import Rattletrap.Type.PickupAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putPickupAttribute :: PickupAttribute -> BinaryBit.BitPut ()
putPickupAttribute pickupAttribute = do
  case pickupAttributeInstigatorId pickupAttribute of
    Nothing -> BinaryBit.putBool False
    Just instigatorId -> do
      BinaryBit.putBool True
      putWord32Bits instigatorId
  BinaryBit.putBool (pickupAttributePickedUp pickupAttribute)
