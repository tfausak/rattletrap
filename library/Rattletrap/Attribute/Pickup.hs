module Rattletrap.Attribute.Pickup where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PickupAttribute = PickupAttribute
  { pickupAttributeInstigatorId :: Maybe Word32
  , pickupAttributePickedUp :: Bool
  } deriving (Eq, Ord, Show)

getPickupAttribute :: BinaryBit.BitGet PickupAttribute
getPickupAttribute = do
  instigator <- BinaryBit.getBool
  maybeInstigatorId <- if instigator
    then do
      instigatorId <- getWord32Bits
      pure (Just instigatorId)
    else pure Nothing
  pickedUp <- BinaryBit.getBool
  pure (PickupAttribute maybeInstigatorId pickedUp)

putPickupAttribute :: PickupAttribute -> BinaryBit.BitPut ()
putPickupAttribute pickupAttribute = do
  case pickupAttributeInstigatorId pickupAttribute of
    Nothing -> BinaryBit.putBool False
    Just instigatorId -> do
      BinaryBit.putBool True
      putWord32Bits instigatorId
  BinaryBit.putBool (pickupAttributePickedUp pickupAttribute)
