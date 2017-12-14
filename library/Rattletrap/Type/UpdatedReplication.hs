{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UpdatedReplication
  ( UpdatedReplication(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Ord, Show)

$(deriveJson ''UpdatedReplication)
