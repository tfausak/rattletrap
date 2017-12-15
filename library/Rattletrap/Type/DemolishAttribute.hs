{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.DemolishAttribute
  ( DemolishAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector
import Rattletrap.Type.Word32le

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32le
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32le
  , demolishAttributeAttackerVelocity :: Vector
  , demolishAttributeVictimVelocity :: Vector
  } deriving (Eq, Ord, Show)

$(deriveJson ''DemolishAttribute)
