{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.PickupAttribute
  ( PickupAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32

data PickupAttribute = PickupAttribute
  { pickupAttributeInstigatorId :: Maybe Word32
  , pickupAttributePickedUp :: Bool
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON PickupAttribute where
  parseJSON = defaultParseJson "PickupAttribute"

instance ToJSON PickupAttribute where
  toEncoding = defaultToEncoding "PickupAttribute"
  toJSON = defaultToJson "PickupAttribute"
