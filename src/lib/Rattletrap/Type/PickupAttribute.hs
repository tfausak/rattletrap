{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PickupAttribute
  ( PickupAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le

data PickupAttribute = PickupAttribute
  { pickupAttributeInstigatorId :: Maybe Word32le
  , pickupAttributePickedUp :: Bool
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''PickupAttribute)
