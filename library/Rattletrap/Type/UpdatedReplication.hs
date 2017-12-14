module Rattletrap.Type.UpdatedReplication
  ( UpdatedReplication(..)
  ) where

import Rattletrap.Attribute

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Ord, Show)
