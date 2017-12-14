module Rattletrap.Type.Replication
  ( Replication(..)
  ) where

import Rattletrap.Type.CompressedWord
import Rattletrap.Type.ReplicationValue

data Replication = Replication
  { replicationActorId :: CompressedWord
  , replicationValue :: ReplicationValue
  } deriving (Eq, Ord, Show)
