{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.DemolishAttribute
  ( DemolishAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Vector

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32
  , demolishAttributeAttackerVelocity :: Vector
  , demolishAttributeVictimVelocity :: Vector
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON DemolishAttribute where
  parseJSON = defaultParseJson "DemolishAttribute"

instance ToJSON DemolishAttribute where
  toEncoding = defaultToEncoding "DemolishAttribute"
  toJSON = defaultToJson "DemolishAttribute"
