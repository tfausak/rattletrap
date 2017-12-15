{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Replication
  ( Replication(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.ReplicationValue

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Replication where
  parseJSON = defaultParseJson "Replication"

instance ToJSON Replication where
  toEncoding = defaultToEncoding "Replication"
  toJSON = defaultToJson "Replication"
