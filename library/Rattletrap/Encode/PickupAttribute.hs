module Rattletrap.Encode.PickupAttribute
  ( putPickupAttribute
  )
where

import Rattletrap.Encode.Word32le
import Rattletrap.Type.PickupAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putPickupAttribute :: PickupAttribute -> BinaryBits.BitPut ()
putPickupAttribute pickupAttribute = do
  case pickupAttributeInstigatorId pickupAttribute of
    Nothing -> BinaryBits.putBool False
    Just instigatorId -> do
      BinaryBits.putBool True
      putWord32Bits instigatorId
  BinaryBits.putBool (pickupAttributePickedUp pickupAttribute)
