module Rattletrap.Type.DemolishAttribute
  ( DemolishAttribute(..)
  ) where

import Rattletrap.Type.Word32
import Rattletrap.Type.Vector

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32
  , demolishAttributeAttackerVelocity :: Vector
  , demolishAttributeVictimVelocity :: Vector
  } deriving (Eq, Ord, Show)
