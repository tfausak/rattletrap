{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UpdatedReplication
  ( UpdatedReplication(..)
  )
where

import Rattletrap.Type.Attribute
import Rattletrap.Type.Common

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Ord, Show)

$(deriveJson ''UpdatedReplication)
