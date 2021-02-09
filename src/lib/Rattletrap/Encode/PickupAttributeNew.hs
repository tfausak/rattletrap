module Rattletrap.Encode.PickupAttributeNew
  ( putPickupAttributeNew
  ) where

import Rattletrap.Encode.Word32le
import Rattletrap.Encode.Word8le
import Rattletrap.Type.PickupAttributeNew

import qualified Data.Binary.Bits.Put as BinaryBits

putPickupAttributeNew :: PickupAttributeNew -> BinaryBits.BitPut ()
putPickupAttributeNew pickupAttributeNew = do
  case pickupAttributeNewInstigatorId pickupAttributeNew of
    Nothing -> BinaryBits.putBool False
    Just instigatorId -> do
      BinaryBits.putBool True
      putWord32Bits instigatorId
  putWord8Bits (pickupAttributeNewPickedUp pickupAttributeNew)
