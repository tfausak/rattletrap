{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PickupAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupAttribute = PickupAttribute
  { pickupAttributeInstigatorId :: Maybe Word32le
  , pickupAttributePickedUp :: Bool
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''PickupAttribute)

putPickupAttribute :: PickupAttribute -> BinaryBits.BitPut ()
putPickupAttribute pickupAttribute = do
  case pickupAttributeInstigatorId pickupAttribute of
    Nothing -> BinaryBits.putBool False
    Just instigatorId -> do
      BinaryBits.putBool True
      putWord32Bits instigatorId
  BinaryBits.putBool (pickupAttributePickedUp pickupAttribute)
