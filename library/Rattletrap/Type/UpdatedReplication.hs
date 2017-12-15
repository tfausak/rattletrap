{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.UpdatedReplication
  ( UpdatedReplication(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON UpdatedReplication where
  parseJSON = defaultParseJson "UpdatedReplication"

instance ToJSON UpdatedReplication where
  toEncoding = defaultToEncoding "UpdatedReplication"
  toJSON = defaultToJson "UpdatedReplication"
