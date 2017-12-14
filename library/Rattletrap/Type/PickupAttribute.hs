module Rattletrap.Type.PickupAttribute
  ( PickupAttribute(..)
  ) where

import Rattletrap.Type.Word32

data PickupAttribute = PickupAttribute
  { pickupAttributeInstigatorId :: Maybe Word32
  , pickupAttributePickedUp :: Bool
  } deriving (Eq, Ord, Show)
