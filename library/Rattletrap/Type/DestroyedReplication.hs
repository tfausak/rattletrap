{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.DestroyedReplication
  ( DestroyedReplication(..)
  ) where

import Rattletrap.Type.Common

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data DestroyedReplication = DestroyedReplication
  deriving (Eq, Generic, Ord, Show)

instance FromJSON DestroyedReplication where
  parseJSON = defaultParseJson "DestroyedReplication"

instance ToJSON DestroyedReplication where
  toEncoding = defaultToEncoding "DestroyedReplication"
  toJSON = defaultToJson "DestroyedReplication"
