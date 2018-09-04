{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.DestroyedReplication
  ( DestroyedReplication(..)
  )
where

import Rattletrap.Type.Common

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data DestroyedReplication = DestroyedReplication
  deriving (Eq, Ord, Show)

$(deriveJson ''DestroyedReplication)
