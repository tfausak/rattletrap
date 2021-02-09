{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PickupAttributeNew
  ( PickupAttributeNew(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

data PickupAttributeNew = PickupAttributeNew
  { pickupAttributeNewInstigatorId :: Maybe Word32le
  , pickupAttributeNewPickedUp :: Word8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''PickupAttributeNew)
