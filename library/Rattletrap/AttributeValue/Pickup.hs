module Rattletrap.AttributeValue.Pickup where

import Rattletrap.Primitive.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PickupAttributeValue = PickupAttributeValue
  { pickupAttributeValueInstigatorId :: Maybe Word32
  , pickupAttributeValuePickedUp :: Bool
  } deriving (Eq, Ord, Show)

getPickupAttributeValue :: BinaryBit.BitGet PickupAttributeValue
getPickupAttributeValue = do
  instigator <- BinaryBit.getBool
  maybeInstigatorId <-
    if instigator
      then do
        instigatorId <- getWord32Bits
        pure (Just instigatorId)
      else pure Nothing
  pickedUp <- BinaryBit.getBool
  pure (PickupAttributeValue maybeInstigatorId pickedUp)

putPickupAttributeValue :: PickupAttributeValue -> BinaryBit.BitPut ()
putPickupAttributeValue pickupAttributeValue = do
  case pickupAttributeValueInstigatorId pickupAttributeValue of
    Nothing -> BinaryBit.putBool False
    Just instigatorId -> do
      BinaryBit.putBool True
      putWord32Bits instigatorId
  BinaryBit.putBool (pickupAttributeValuePickedUp pickupAttributeValue)
